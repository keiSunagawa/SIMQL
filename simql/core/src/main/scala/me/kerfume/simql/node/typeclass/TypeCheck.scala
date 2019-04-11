package me.kerfume.simql.node.typeclass

import cats.instances.list._
import me.kerfume.simql._
import me.kerfume.simql.functions._
import me.kerfume.simql.node.SIMQLFunction.TypeMap
import me.kerfume.simql.node._
import simulacrum._
import TypeTree._

@typeclass trait TypeCheck[A] {
  def check(a: A, scope: Scope, paramMap: TypeMap): Result[SIMQLType]
}

object TypeCheck {
  def instance[A](f: (A, Scope, TypeMap) => Result[SIMQLType]): TypeCheck[A] = new TypeCheck[A] {
    override def check(a: A, scope: Scope, paramMap: TypeMap): Result[SIMQLType] = f(a, scope, paramMap)
  }
  implicit val numberTC: TypeCheck[NumberLit] = instance { (_, _, _) =>
    Right(NumberType)
  }
  implicit val stringTC: TypeCheck[StringLit] = instance { (_, _, _) =>
    Right(StringType)
  }
  implicit val symbolTC: TypeCheck[SymbolLit] = instance { (_, _, _) =>
    Right(SymbolType)
  }
  implicit val booleanTC: TypeCheck[BooleanLit] = instance { (_, _, _) =>
    Right(BooleanType)
  }
  implicit val rawTC: TypeCheck[Raw] = instance { (a, scope, paramMap) =>
    for {
      _ <- a.args.mapE { TypeCheck[Expr].check(_, scope, paramMap) }
    } yield RawType
  }
  implicit val bexprTC: TypeCheck[BExpr] = instance { (a, scope, paramMap) =>
    for {
      _ <- TypeCheck[Expr].check(a.rhs, scope, paramMap)
      _ <- TypeCheck[Expr].check(a.lhs, scope, paramMap)
    } yield ExprType
  }
  implicit val rbracketTC: TypeCheck[RBracket] = instance { (a, scope, paramMap) =>
    for {
      _ <- TypeCheck[Expr].check(a.expr, scope, paramMap)
    } yield ExprType
  }
  implicit val callTc: TypeCheck[Call] = instance { (a, scope, paramMap) =>
    def typeMismatch(found: SIMQLType, require: SIMQLType): SIMQLError = UnhandleError(s"type mismatch. key: ${a.symbol}, found: $found, require: $require")
    def probeGenerics(pTree: TypeTree, aTree: TypeTree): Option[Map[Generics, SIMQLType]] = {
      (pTree, aTree) match {
        case (Has0(g: Generics), resolveTo) => Some(Map(g -> resolveTo.toType))
        case (Has0(_), _) => Some(Map.empty)
        case (Has1(_, pTree1), Has1(_, aTree1)) => probeGenerics(pTree1, aTree1)
        case (Has2(_, pTree1, pTree2), Has2(_, aTree1, aTree2)) =>
          for {
            m1 <- probeGenerics(pTree1, aTree1)
            m2 <- probeGenerics(pTree2, aTree2)
          } yield m1 ++ m2
        case _ => None
      }
    }
    def replaceGenerics(tree: TypeTree, gmap: Map[Generics, SIMQLType]): SIMQLType = {
      def replace(tree: TypeTree): TypeTree = {
        tree match {
          case Has0(g: Generics) =>
            val replaced = gmap.getOrElse(g, g) // when unresolved generics, take identity
            Has0(replaced)
          case other: Has0 => other
          case Has1(c, tpe) => Has1(c, replace(tpe))
          case Has2(c, tpe1, tpe2) => Has2(c, replace(tpe1), replace(tpe2))
        }
      }
      replace(tree).toType
    }
    def refTypeCheck(tpe: SIMQLType, typeArgs: List[SIMQLType]): Result[SIMQLType] = tpe match {
      case f: FunctionType =>
        typeArgs.foldE[SIMQLError, SIMQLType](f) { case (tpe, arg) =>
          tpe match {
            case f: FunctionType =>
              val paramTree = ToTypeTree[SIMQLType].toTree(f.paramType)
              val argTree = ToTypeTree[SIMQLType].toTree(arg)
              val error = typeMismatch(f.paramType, arg)
              for {
                genericsMap <- probeGenerics(paramTree, argTree).toRight(error)
                resolvedP = replaceGenerics(paramTree, genericsMap)
                _ <- Either.cond(resolvedP == arg, (), error)
              } yield replaceGenerics(ToTypeTree[SIMQLType].toTree(f.returnType), genericsMap)
            case _ => Left(UnhandleError("args to many."))
         }
        }
      case _ => Either.cond(typeArgs.isEmpty, tpe, UnhandleError("var not apply args."))
    }
    for {
      typeArgs <- a.args.mapE(TypeCheck[Expr].check(_, scope, paramMap))
      tpe <- scope
              .get(a.symbol)
              .map { v =>
                for {
                  res <- TypeCheck[Value].check(v, scope, paramMap)
                  res2 <- refTypeCheck(res, typeArgs)
                } yield res2
              }
              .orElse(paramMap.get(a.symbol).map(refTypeCheck(_, typeArgs)))
              .toRight(UnhandleError(s"type check: function not found. key: ${a.symbol}"))
              .flatMap(identity) // ひどい…
    } yield tpe
  }
  implicit val listTC: TypeCheck[SIMQLList] = instance { (a, scope, paramMap) =>
    a.elems
      .foldE(a.elemType) {
        case (tpe, e) =>
          for {
            etpe <- TypeCheck[Expr].check(e, scope, paramMap)
            _ <- Either.cond(tpe == etpe, (), UnhandleError("list type error."))
          } yield tpe
      }
      .map(ListType)
  }
  implicit val bfTC: TypeCheck[BuildInFunction] = instance { (a, _, _) =>
    Right(FunctionType(a.param.tpe, a.returnType))
  }
  implicit val ufTC: TypeCheck[UserFunction] = instance { (a, scope, paramMap) =>
    val paramMap2 = paramMap + (a.param.name -> a.param.tpe)
    val lscope = scope ++ a.outerScope
    for {
      binded <- a.body.foldE[SIMQLError, TypeMap](paramMap2) {
                 case (acm, b) =>
                   TypeCheck[Expr].check(b.value, lscope, acm).map(tpe => acm + (b.symbol -> tpe))
               }
      tpe <- TypeCheck[Expr].check(a.returnValue, scope, binded)
      _ <- Either.cond(
            a.returnType.isSameType(tpe),
            (),
            UnhandleError(s"function return type error. key: ${a.key}, found: ${tpe}, require: ${a.returnType}")
          )
    } yield FunctionType(a.param.tpe, a.returnType)
  }
  implicit val functionTC: TypeCheck[SIMQLFunction] = instance { (a, scope, paramMap) =>
    a match {
      case f: BuildInFunction => TypeCheck[BuildInFunction].check(f, scope, paramMap)
      case f: UserFunction    => TypeCheck[UserFunction].check(f, scope, paramMap)
    }
  }

  implicit val exprTC: TypeCheck[Expr] = instance { (a, scope, paramMap) =>
    a match {
      case n: NumberLit     => TypeCheck[NumberLit].check(n, scope, paramMap)
      case n: StringLit     => TypeCheck[StringLit].check(n, scope, paramMap)
      case n: SymbolLit     => TypeCheck[SymbolLit].check(n, scope, paramMap)
      case n: BooleanLit    => TypeCheck[BooleanLit].check(n, scope, paramMap)
      case NullLit          => throw new RuntimeException("null type can't type check.")
      case n: Call          => TypeCheck[Call].check(n, scope, paramMap)
      case n: SIMQLList     => TypeCheck[SIMQLList].check(n, scope, paramMap)
      case n: SIMQLFunction => TypeCheck[SIMQLFunction].check(n, scope, paramMap)
      case n: Raw           => TypeCheck[Raw].check(n, scope, paramMap)
      case n: BExpr         => TypeCheck[BExpr].check(n, scope, paramMap)
      case n: RBracket      => TypeCheck[RBracket].check(n, scope, paramMap)
    }
  }

  implicit val thunkTC: TypeCheck[Thunk] = instance { (a, scope, paramMap) =>
    TypeCheck[Expr].check(a.value, scope, paramMap)
  }
  implicit val pureTC: TypeCheck[Pure] = instance { (a, scope, paramMap) =>
    TypeCheck[Expr].check(a.value, scope, paramMap)
  }
  implicit val valueTC: TypeCheck[Value] = instance { (a, scope, paramMap) =>
    a match {
      case v: Thunk => TypeCheck[Thunk].check(v, scope, paramMap)
      case v: Pure  => TypeCheck[Pure].check(v, scope, paramMap)
    }
  }
}
