package me.kerfume.simql.node

import me.kerfume.simql._
import me.kerfume.simql.types._
import me.kerfume.simql.functions._
import cats.instances.list._
import SIMQLFunction._

sealed trait ASTNode
sealed trait FunctionAST extends ASTNode
sealed trait QueryAST extends ASTNode

sealed trait Atomic extends Expr {
  def eval(scope: Scope, ctx: QueryContext): Result[Expr] = Right(this)
}

case class StringLit(value: String) extends Atomic {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = Right(StringType)
}
case class NumberLit(value: BigDecimal) extends Atomic {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = Right(NumberType)
}
case class BooleanLit(value: Boolean) extends Atomic {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = Right(BooleanType)
}
case object NullLit extends Atomic {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = ??? // TODO
}
case class SymbolLit(label: String) extends Atomic {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = Right(SymbolType)
}

sealed trait Expr extends QueryAST with FunctionAST {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType]
  def eval(scope: Scope, ctx: QueryContext): Result[Expr]
}

case class Call(symbol: String, args: List[Expr]) extends QueryAST with FunctionAST with Expr {
  private[this] def refTypeCheck(tpe: SIMQLType, typeArgs: List[SIMQLType]): Result[SIMQLType] = tpe match {
    case f: FunctionType =>
      for {
        _ <- Either.cond(typeArgs.nonEmpty, (), UnhandleError("function require to args."))
        ret <- typeArgs.foldE[SIMQLError, SIMQLType](f) {
                case (tpe, arg) =>
                  tpe match {
                    case f: FunctionType =>
                      val f2 =
                        if (f.paramType == Generics || f.paramType == ListType(Generics)) {
                          f.resolveGenerics(arg)
                        } else f
                      Either.cond(
                        f2.paramType.isSameType(arg),
                        f2.returnType,
                        UnhandleError(
                          s"unmatch args type. key: $symbol, found: ${arg}, require: ${f2.paramType}"
                        )
                      )
                    case _ => Left(UnhandleError("args to many."))
                  }
              }
      } yield ret
    case _ => Either.cond(typeArgs.isEmpty, tpe, UnhandleError("var not apply args."))
  }
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = {
    for {
      typeArgs <- args.mapE(_.typeCheck(scope, paramMap))
      tpe <- scope
              .get(symbol)
              .map { f =>
                for {
                  res <- f match {
                          case v: Thunk => v.typeCheck(paramMap)
                          case v: Pure  => v.typeCheck(scope, paramMap)
                        }
                  res2 <- refTypeCheck(res, typeArgs)
                } yield res2
              }
              .orElse(paramMap.get(symbol).map(refTypeCheck(_, typeArgs)))
              .toRight(UnhandleError(s"type check: function not found. key: $symbol"))
              .flatMap(identity) // ひどい…
    } yield {
      tpe match {
        case Generics =>
          println(s"== ret generics to $symbol ==")
          tpe
        case other => other
      }
    }
  }

  override def eval(scope: Scope, ctx: QueryContext): Result[Expr] = {
    for {
      f <- scope.get(symbol).toRight(FunctionNotFound(symbol))
      invalue <- f match {
                  case v: Thunk => v.eval(ctx)
                  case v: Pure  => v.eval(scope, ctx)
                }
      res <- invalue match {
              case ff: SIMQLFunction => ff.setOuterScope(scope).apply(args.map(Thunk(_, scope)), ctx) // FIXME 実装が怪しい
              case other             => Right(other)
            }
    } yield res
  }
}

case class Raw(sql: String, args: List[Expr]) extends Expr {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = Right(RawType)
  def eval(scope: Scope, ctx: QueryContext): Result[Expr] = {
    for {
      evaled <- args.mapE(_.eval(scope, ctx))
    } yield
      this.copy(
        args = evaled
      )
  }
}

case class Op(op: ExprOp.Op) extends QueryAST

// resolved operation priority. by making syntax tree.
case class BExpr(lhs: Expr, op: Op, rhs: Expr) extends Expr {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] =
    for {
      _ <- lhs.typeCheck(scope, paramMap)
      _ <- rhs.typeCheck(scope, paramMap)
    } yield ExprType

  def eval(scope: Scope, ctx: QueryContext): Result[Expr] = {
    for {
      evaledLhs <- lhs.eval(scope, ctx)
      evaledRhs <- rhs.eval(scope, ctx)
    } yield
      this.copy(
        lhs = evaledLhs,
        rhs = evaledRhs
      )
  }
}

case class RBracket(expr: Expr) extends Expr {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = expr.typeCheck(scope, paramMap).map(_ => ExprType)
  def eval(scope: Scope, ctx: QueryContext): Result[Expr] = {
    for {
      evaled <- expr.eval(scope, ctx)
    } yield
      this.copy(
        expr = evaled
      )
  }
}

case class JoinType(value: JoinType.Op) extends QueryAST
case class Join(joinType: JoinType, rhsTable: Expr, on: Expr) extends QueryAST // real allowed to column | row | subQuery

case class OrderType(value: OrderType.Op) extends QueryAST

case class From(lhs: Expr, rhss: List[Join]) extends QueryAST // real allowed to column | row | subQuery
case class Select(columns: NEL[Expr]) extends QueryAST // real allowed to column | row
case class Where(expr: Expr) extends QueryAST
case class LimitOffset(limit: NumberLit, offset: Option[NumberLit]) extends QueryAST
case class Order(orderType: OrderType, columns: NEL[Expr]) extends QueryAST // real allowed to column | row

case class Query(
  from: From,
  select: Option[Select],
  where: Option[Where],
  limitOffset: Option[LimitOffset],
  order: Option[Order])
    extends QueryAST

object ExprOp {
  sealed trait Op
  case object GT extends Op
  case object LT extends Op
  case object GE extends Op
  case object LE extends Op
  case object EQ extends Op
  case object NE extends Op

  case object And extends Op
  case object Or extends Op
}

object JoinType {
  sealed trait Op
  case object LeftJoin extends Op
  case object InnerJoin extends Op
}

object OrderType {
  sealed trait Op
  case object Asc extends Op
  case object Desc extends Op
}

// for function ast
case class Bind(symbol: String, value: Expr) extends FunctionAST {
  def bind(scope: Scope, ctx: QueryContext): Scope = {
    scope + (symbol -> Thunk(value, scope))
  }
}

sealed trait Value {
  val value: Expr
  protected[this] def typeCheck0(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = value.typeCheck(scope, paramMap)
  protected[this] def eval0(scope: Scope, ctx: QueryContext): Result[Expr] = value.eval(scope, ctx)
}
case class Thunk(value: Expr, scope: Scope) extends Value {
  def typeCheck(paramMap: TypeMap): Result[SIMQLType] = {
    typeCheck0(scope, paramMap)
  }
  def eval(ctx: QueryContext): Result[Expr] = eval0(scope, ctx)
}
case class Pure(value: Expr) extends Value {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = typeCheck0(scope, paramMap)
  def eval(scope: Scope, ctx: QueryContext): Result[Expr] = eval0(scope, ctx)
}
sealed trait SIMQLType {
  val subType: List[SIMQLType] = Nil
  def isSameType(that: SIMQLType): Boolean = this == that || subType.contains(that) || that == Generics
}
sealed trait ElemType extends SIMQLType
case object StringType extends ElemType
case object BooleanType extends ElemType
case object SymbolType extends ElemType
case object NumberType extends ElemType
case object RawType extends ElemType
case object ExprType extends ElemType {
  override val subType = List(StringType, BooleanType, SymbolType, NumberType, RawType)
}
case class ListType(elemType: SIMQLType) extends SIMQLType
case class FunctionType(paramType: SIMQLType, returnType: SIMQLType) extends SIMQLType {
  def resolveGenerics(tpe: SIMQLType): FunctionType = tpe match {
    case Generics => throw new RuntimeException("invalid input can't resolve generics in Generics")
    case other =>
      val param = paramType match {
        case Generics           => other
        case ListType(Generics) => ListType(other)
        case prm                => prm
      }
      val ret = returnType match {
        case Generics           => other
        case f: FunctionType    => f.resolveGenerics(other)
        case ListType(Generics) => ListType(other)
        case prm                => prm
      }
      FunctionType(param, ret)
  }
}
case object Generics extends SIMQLType {
  override def isSameType(that: SIMQLType): Boolean = true
}

// TODO function callを要素として持ちうる？cons関数で駆らなずevalするのでないもとして扱うべきか
case class SIMQLList(elems: List[Expr], elemType: SIMQLType) extends Atomic {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = {
    elems.foldE(ListType(elemType)) {
      case (tpe, e) =>
        for {
          etpe <- e.typeCheck(scope, paramMap)
          _ <- Either.cond(tpe.elemType == etpe, (), UnhandleError("list type error."))
        } yield tpe
    }
  }
}
import SIMQLFunction._

trait SIMQLFunction extends Atomic { self =>
  val key: String
  val param: FunctionParam
  val returnType: SIMQLType
  val outerScope: Scope

  val body: List[Bind]
  val returnValue: Expr

  def setOuterScope(scope: Scope): SIMQLFunction = new SIMQLFunction {
    override val key: String = self.key
    override val param: FunctionParam = self.param
    override val returnType: SIMQLType = self.returnType
    override val outerScope: Scope = self.outerScope ++ scope
    override val body: List[Bind] = self.body
    override val returnValue: Expr = self.returnValue

    override def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr] =
      self.apply0(scope, ctx, nextArgs)

    override def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = self.typeCheck(scope, paramMap)
  }

  final def apply(args: List[Value], ctx: QueryContext): Result[Expr] = {
    for {
      arg <- args.headOption.toRight(UnhandleError(s"args empty. key: $key"))
      scope = ctx.globalScope ++ outerScope + (param.name -> arg)
      res <- apply0(scope, ctx, args.tail)
    } yield res
  }

  def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr]
}

trait UserFunction extends SIMQLFunction {
  def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] = {
    val paramMap2 = paramMap + (param.name -> param.tpe)
    val lscope = scope ++ outerScope
    for {
      binded <- body.foldE[SIMQLError, TypeMap](paramMap2) {
                 case (acm, b) =>
                   b.value.typeCheck(lscope, acm).map(tpe => acm + (b.symbol -> tpe))
               }
      tpe <- returnValue.typeCheck(scope, binded)
      _ <- Either.cond(returnType.isSameType(tpe), (), UnhandleError(s"function return type error. key: $key"))
    } yield FunctionType(param.tpe, returnType)
  }

  def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr] = {
    val binded = body.foldLeft(scope) {
      case (acm, b) => b.bind(acm, ctx)
    }
    returnValue.eval(binded, ctx).flatMap {
      case next: SIMQLFunction =>
        if (nextArgs.nonEmpty) next.setOuterScope(binded)(nextArgs, ctx)
        else Right(next)
      case other => Right(other)
    }
  }
}

case class Closure(
  key: String = "__anonymous",
  param: FunctionParam,
  body: List[Bind],
  returnType: SIMQLType,
  returnValue: Expr,
  outerScope: Scope = Map.empty)
    extends UserFunction

object SIMQLFunction {
  case class FunctionParam(name: String, tpe: SIMQLType)
  type TypeMap = Map[String, SIMQLType]

  type CheckScope = Map[String, SIMQLType]
  case class DefunType(params: Seq[SIMQLType], ret: SIMQLType)

  case class TypeError(fname: String, found: SIMQLType, require: SIMQLType) extends SIMQLError
  case class ReturnTypeError(key: String, found: SIMQLType, require: SIMQLType) extends SIMQLError
  case class UnmatchArgLength() extends SIMQLError
}
