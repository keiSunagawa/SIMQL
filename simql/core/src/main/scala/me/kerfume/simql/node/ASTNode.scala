package me.kerfume.simql.node

import me.kerfume.simql._
import me.kerfume.simql.types._
import me.kerfume.simql.functions._
import cats.instances.list._
import SIMQLFunction._
import me.kerfume.simql.node.typeclass.Eval

sealed trait ASTNode
sealed trait FunctionAST extends ASTNode
sealed trait QueryAST extends ASTNode

sealed trait Expr extends QueryAST with FunctionAST
sealed trait Atomic extends Expr

case class NumberLit(value: BigDecimal) extends Atomic
case class StringLit(value: String) extends Atomic
case class SymbolLit(label: String) extends Atomic
case class BooleanLit(value: Boolean) extends Atomic
case object NullLit extends Atomic

case class Call(symbol: String, args: List[Expr]) extends QueryAST with FunctionAST with Expr
case class Raw(sql: String, args: List[Expr]) extends Expr

case class Op(op: ExprOp.Op) extends QueryAST

// resolved operation priority. by making syntax tree.
case class BExpr(lhs: Expr, op: Op, rhs: Expr) extends Expr
case class RBracket(expr: Expr) extends Expr

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

sealed trait Value
case class Thunk(value: Expr, scope: Scope) extends Value
case class Pure(value: Expr) extends Value
sealed trait SIMQLType {
  val subType: List[SIMQLType] = Nil
  def isSameType(that: SIMQLType): Boolean = this == that || subType.contains(that)
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
case class FunctionType(paramType: SIMQLType, returnType: SIMQLType) extends SIMQLType
case class Generics(symbol: String) extends SIMQLType

case class SIMQLList(elems: List[Expr], elemType: SIMQLType) extends Atomic

import SIMQLFunction._

sealed trait SIMQLFunction extends Atomic { self =>
  val key: String
  val param: FunctionParam
  val returnType: SIMQLType
  val outerScope: Scope
  val defGenerics: List[Generics]

  val body: List[Bind]
  val returnValue: Expr

  def setOuterScope(scope: Scope): SIMQLFunction = new UserFunction { // SIMQLFunctionの型網羅性を保持するためにUserFunctionでnew
    override val key: String = self.key
    override val param: FunctionParam = self.param
    override val returnType: SIMQLType = self.returnType
    override val outerScope: Scope = self.outerScope ++ scope
    override val body: List[Bind] = self.body
    override val returnValue: Expr = self.returnValue
    override val defGenerics = self.defGenerics

    override def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr] =
      self.apply0(scope, ctx, nextArgs)
  }

  final def apply(args: List[Value], ctx: QueryContext): Result[Expr] = {
    args.headOption match {
      case Some(arg) =>
        val scope = ctx.globalScope ++ outerScope + (param.name -> arg)
        apply0(scope, ctx, args.tail)
      case None => Right(this)
    }
  }

  def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr]
}

trait BuildInFunction extends SIMQLFunction {
  val body: List[Bind] = Nil
  val returnValue: Expr = NullLit
  val outerScope: Scope = Map.empty
}

trait UserFunction extends SIMQLFunction {
  def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr] = {
    val binded = body.foldLeft(scope) {
      case (acm, b) => b.bind(acm, ctx)
    }
    Eval[Expr].eval(returnValue, binded, ctx).flatMap {
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
  defGenerics: List[Generics],
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
