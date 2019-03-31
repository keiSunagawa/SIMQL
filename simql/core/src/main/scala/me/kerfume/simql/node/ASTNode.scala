package me.kerfume.simql.node

import me.kerfume.simql._
import me.kerfume.simql.types._
import me.kerfume.simql.functions._
import cats.instances.list._

sealed trait ASTNode
sealed trait FunctionAST extends ASTNode
sealed trait QueryAST extends ASTNode
sealed trait EvalNode { self: FunctionAST =>
  def eval(scope: Scope, ctx: QueryContext): Result[Expr]
}
trait Leaf { self: EvalNode with Expr =>
  def eval(scope: Scope, ctx: QueryContext): Result[Expr] = Right(this)
}

sealed trait Expr extends QueryAST with FunctionAST with EvalNode

case class FunctionCall(symbol: String, args: List[Expr]) extends QueryAST with FunctionAST with Expr {

  override def eval(scope: Scope, ctx: QueryContext): Result[Expr] = {
    for {
      f <- scope.get(symbol).toRight(FunctionNotFound(symbol))
      res <- f(args, scope, ctx)
    } yield res
  }
}

case class StringLit(value: String) extends Expr with Leaf
case class NumberLit(value: BigDecimal) extends Expr with Leaf
case object NullLit extends Expr with Leaf
case class SymbolLit(label: String) extends Expr with Leaf
case class Raw(sql: String, args: List[Expr]) extends Expr {
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
  private[this] def caseEval(scope: Scope, ctx: QueryContext): Unit =
    value match { // FIXME: unsafe and known eval function by Bind class
      case f: FunctionCall =>
        if (f.symbol == "eval") {
          f.eval(scope, ctx)
        }
      case _ => ()
    }
  def bind(scope: Scope, ctx: QueryContext): Scope = {
    caseEval(scope, ctx)
    scope + (symbol -> new Variable(symbol, value, scope))
  }
}

sealed trait FunctionParam {
  val key: String
  val tpe: FunctionReturnType
  type Actual <: Expr

  def resolve0: PartialFunction[Expr, Actual]
  def resolve(arg: Expr, outerScope: Scope, ctx: QueryContext): Result[(String, Actual)] = {
    for {
      evaled <- arg.eval(outerScope, ctx)
      resolved <- resolve0.lift(evaled).map(key -> _).toRight(FunctionParamTypeError(this, arg))
    } yield resolved
  }
}
case class StringParam(key: String) extends FunctionParam {
  override type Actual = StringLit
  val tpe = StringType
  def resolve0 = { case e: StringLit => e }
}
case class NumberParam(key: String) extends FunctionParam {
  override type Actual = NumberLit
  val tpe = NumberType
  def resolve0 = { case e: NumberLit => e }
}
case class SymbolParam(key: String) extends FunctionParam {
  override type Actual = SymbolLit
  val tpe = SymbolType
  def resolve0 = { case e: SymbolLit => e }
}
case class RawParam(key: String) extends FunctionParam {
  override type Actual = Raw
  val tpe = RawType
  def resolve0 = { case e: Raw => e }
}
case class ExprParam(key: String) extends FunctionParam { // pend implements
  override type Actual = Expr
  val tpe = ExprType
  def resolve0 = { case e: Expr => e }
}

sealed trait FunctionReturnType {
  def isAllowed(that: FunctionReturnType): Boolean
}
trait LeafType { self: FunctionReturnType =>
  def isAllowed(that: FunctionReturnType): Boolean = that == this
}
case object StringType extends FunctionReturnType with LeafType
case object NumberType extends FunctionReturnType with LeafType
case object SymbolType extends FunctionReturnType with LeafType
case object RawType extends FunctionReturnType with LeafType
case object ExprType extends FunctionReturnType {
  def isAllowed(that: FunctionReturnType): Boolean = that match {
    case StringType | NumberType | SymbolType | RawType | ExprType => true
  }
}
object FunctionReturnType {
  def fromExpr(expr: Expr, scope: Scope): FunctionReturnType = expr match {
    case _: StringLit    => StringType
    case _: NumberLit    => NumberType
    case _: SymbolLit    => SymbolType
    case _: Raw          => RawType
    case f: FunctionCall => scope(f.symbol).returnType
    case _: Expr         => ExprType
  }
}

trait SIMQLFunction {
  val key: String
  val params: List[FunctionParam]
  val returnType: FunctionReturnType
  val body: List[Bind]
  val returnExpr: Expr // TODO safe un function Expr

  def apply(args: List[Expr], outerScope: Scope, ctx: QueryContext): Result[Expr] = {
    val resolvedArgs = params.zip(args).map { case (p, a) => p.key -> a }
    val argScope: Scope = resolvedArgs.map { case (k, v) => k -> new Variable(k, v, outerScope) }(collection.breakOut)
    val scope = outerScope ++ argScope
    for {
      result <- apply0(scope, ctx)
    } yield result
  }

  protected[this] def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
    val binded = body.foldLeft(scope) {
      case (acm, b) => b.bind(acm, ctx)
    }
    for {
      result <- returnExpr.eval(binded, ctx)
    } yield result
  }
}

class Variable(val key: String, expr: Expr, outerScope: Scope) extends SIMQLFunction {
  val params: List[FunctionParam] = Nil
  val returnType: FunctionReturnType = FunctionReturnType.fromExpr(expr, outerScope)
  val body: List[Bind] = Nil
  val returnExpr: Expr = expr

  override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
    for {
      result <- returnExpr.eval(outerScope, ctx)
    } yield result
  }
}

object SIMQLFunction {
  def checkFunctionCall(f: FunctionCall, scope: Scope): Result[FunctionReturnType] = {
    val defun = scope(f.symbol)
    for {
      _ <- Either.cond(f.args.length == defun.params.length, (), UnmatchArgLength())
      _ <- f.args.zip(defun.params).zipWithIndex.mapE {
            case ((ff: FunctionCall, p), i) =>
              checkFunctionCall(ff, scope).flatMap { ret =>
                if (isSameType(ret, p)) Right(())
                else Left(TypeError(f.symbol, i, ff.symbol, p))
              }
            case ((a, p), i) =>
              val ret = FunctionReturnType.fromExpr(a, Map.empty)
              if (isSameType(ret, p)) Right(())
              else Left(TypeError(f.symbol, i, a.toString, p))
          }
    } yield defun.returnType
  }

  private[this] def isSameType(arg: FunctionReturnType, param: FunctionParam): Boolean = {
    (arg, param) match {
      case (StringType, _: StringParam) => true
      case (NumberType, _: NumberParam) => true
      case (SymbolType, _: SymbolParam) => true
      case (RawType, _: Raw)            => true
      case (_, _: ExprParam)            => true
      case _                            => false
    }
  }

  case class TypeError(
    fname: String,
    index: Int,
    found: String,
    require: FunctionParam)
      extends SIMQLError
  case class ReturnTypeError(key: String, found: FunctionReturnType, require: FunctionReturnType) extends SIMQLError
  case class UnmatchArgLength() extends SIMQLError
}
