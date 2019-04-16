package me.kerfume.simql.node.typeclass

import cats.instances.list._
import me.kerfume.simql._
import me.kerfume.simql.functions._
import me.kerfume.simql.node.SIMQLFunction.TypeMap
import me.kerfume.simql.node._
import simulacrum._
import TypeTree._

@typeclass trait TypeCheck[A] {
  def check(a: A, paramMap: TypeMap): Result[SIMQLType]
}

object TypeCheck {
  def instance[A](f: (A, TypeMap) => Result[SIMQLType]): TypeCheck[A] = new TypeCheck[A] {
    override def check(a: A, paramMap: TypeMap): Result[SIMQLType] = f(a, paramMap)
  }
  implicit val nullTC: TypeCheck[NullLit.type] = instance { (_, _) =>
    Right(NumberType)
  }
  implicit val numberTC: TypeCheck[NumberLit] = instance { (_, _) =>
    Right(NumberType)
  }
  implicit val stringTC: TypeCheck[StringLit] = instance { (_, _) =>
    Right(StringType)
  }
  implicit val symbolTC: TypeCheck[SymbolLit] = instance { (_, _) =>
    Right(SymbolType)
  }
  implicit val booleanTC: TypeCheck[BooleanLit] = instance { (_, _) =>
    Right(BooleanType)
  }
  implicit val rawTC: TypeCheck[Raw] = instance { (a, paramMap) =>
    for {
      _ <- a.args.mapE { TypeCheck[Expr].check(_, paramMap) }
    } yield RawType
  }
  implicit val bexprTC: TypeCheck[BExpr] = instance { (a, paramMap) =>
    for {
      _ <- TypeCheck[Expr].check(a.rhs, paramMap)
      _ <- TypeCheck[Expr].check(a.lhs, paramMap)
    } yield ExprType
  }
  implicit val rbracketTC: TypeCheck[RBracket] = instance { (a, paramMap) =>
    for {
      _ <- TypeCheck[Expr].check(a.expr, paramMap)
    } yield ExprType
  }
  implicit val callTc: TypeCheck[Call] = instance { (a, paramMap) =>
    def typeMismatch(found: SIMQLType, require: SIMQLType): SIMQLError =
      UnhandleError(s"type mismatch. key: ${a.symbol}, found: $found, require: $require")
    def probeGenerics(pTree: TypeTree, aTree: TypeTree): Option[Map[Generics, SIMQLType]] = {
      (pTree, aTree) match {
        case (Has0(g: Generics), resolveTo)     => Some(Map(g -> resolveTo.toType))
        case (Has0(_), _)                       => Some(Map.empty)
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
            val replaced = gmap.getOrElse(g, g) // when can't resolved generics, take identity.
            Has0(replaced)
          case other: Has0         => other
          case Has1(c, tpe)        => Has1(c, replace(tpe))
          case Has2(c, tpe1, tpe2) => Has2(c, replace(tpe1), replace(tpe2))
        }
      }
      replace(tree).toType
    }
    def refTypeCheck(tpe: SIMQLType, typeArgs: List[SIMQLType]): Result[SIMQLType] = tpe match {
      case f: FunctionType =>
        typeArgs.foldE[SIMQLError, SIMQLType](f) {
          case (tpe, arg) =>
            tpe match {
              case f: FunctionType =>
                val paramTree = ToTypeTree[SIMQLType].toTree(f.paramType)
                val argTree = ToTypeTree[SIMQLType].toTree(arg)
                val error = typeMismatch(arg, f.paramType)
                for {
                  genericsMap <- probeGenerics(paramTree, argTree).toRight(error)
                  resolvedP = replaceGenerics(paramTree, genericsMap)
                  _ <- Either.cond(resolvedP.isSameType(arg), (), error)
                } yield replaceGenerics(ToTypeTree[SIMQLType].toTree(f.returnType), genericsMap)
              case _ => Left(UnhandleError("args to many."))
            }
        }
      case _ => Either.cond(typeArgs.isEmpty, tpe, UnhandleError("var not apply args."))
    }
    for {
      typeArgs <- a.args.mapE(TypeCheck[Expr].check(_, paramMap))
      tpe <- paramMap.get(a.symbol).toRight(UnhandleError(s"type check: function not found. key: ${a.symbol}"))
      res <- refTypeCheck(tpe, typeArgs)
    } yield res
  }
  implicit val listTC: TypeCheck[SIMQLList] = instance { (a, paramMap) =>
    a.elems
      .foldE(a.elemType) {
        case (tpe, e) =>
          for {
            etpe <- TypeCheck[Expr].check(e, paramMap)
            _ <- Either.cond(tpe.isSameType(etpe), (), UnhandleError("list type error."))
          } yield tpe
      }
      .map(ListType)
  }
  implicit val bfTC: TypeCheck[BuildInFunction] = instance { (a, _) =>
    Right(FunctionType(a.param.tpe, a.returnType))
  }
  implicit val ufTC: TypeCheck[UserFunction] = instance { (a, paramMap) =>
    val paramMap2 = paramMap + (a.param.name -> a.param.tpe)
    for {
      binded <- a.body.foldE[SIMQLError, TypeMap](paramMap2) {
                 case (acm, b) =>
                   TypeCheck[Expr].check(b.value, acm).map(tpe => acm + (b.symbol -> tpe))
               }
      tpe <- TypeCheck[Expr].check(a.returnValue, binded)
      _ <- Either.cond(
            a.returnType.isSameType(tpe),
            (),
            UnhandleError(s"function return type error. key: ${a.key}, found: ${tpe}, require: ${a.returnType}")
          )
    } yield FunctionType(a.param.tpe, a.returnType)
  }
  implicit val functionTC: TypeCheck[SIMQLFunction] = instance { (a, paramMap) =>
    a match {
      case f: BuildInFunction => TypeCheck[BuildInFunction].check(f, paramMap)
      case f: UserFunction    => TypeCheck[UserFunction].check(f, paramMap)
    }
  }

  implicit val exprTC: TypeCheck[Expr] = instance { (a, paramMap) =>
    a match {
      case n: NumberLit     => TypeCheck[NumberLit].check(n, paramMap)
      case n: StringLit     => TypeCheck[StringLit].check(n, paramMap)
      case n: SymbolLit     => TypeCheck[SymbolLit].check(n, paramMap)
      case n: BooleanLit    => TypeCheck[BooleanLit].check(n, paramMap)
      case NullLit          => TypeCheck[NullLit.type].check(NullLit, paramMap)
      case n: Call          => TypeCheck[Call].check(n, paramMap)
      case n: SIMQLList     => TypeCheck[SIMQLList].check(n, paramMap)
      case n: SIMQLFunction => TypeCheck[SIMQLFunction].check(n, paramMap)
      case n: Raw           => TypeCheck[Raw].check(n, paramMap)
      case n: BExpr         => TypeCheck[BExpr].check(n, paramMap)
      case n: RBracket      => TypeCheck[RBracket].check(n, paramMap)
    }
  }

  implicit val thunkTC: TypeCheck[Thunk] = instance { (a, paramMap) =>
    TypeCheck[Expr].check(a.value, paramMap)
  }
  implicit val pureTC: TypeCheck[Pure] = instance { (a, paramMap) =>
    TypeCheck[Expr].check(a.value, paramMap)
  }
  implicit val valueTC: TypeCheck[Value] = instance { (a, paramMap) =>
    a match {
      case v: Thunk => TypeCheck[Thunk].check(v, paramMap)
      case v: Pure  => TypeCheck[Pure].check(v, paramMap)
    }
  }
}
