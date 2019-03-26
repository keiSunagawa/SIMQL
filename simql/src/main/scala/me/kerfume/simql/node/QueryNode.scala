package me.kerfume.simql.node

sealed trait QueryNode

object QueryNode {
  sealed trait MacroArg { self: QueryNode => }
  sealed trait Cond extends QueryNode with MacroArg
  sealed trait Cond0 extends Cond

  sealed trait Term extends Cond0
  case class StringWrapper(value: String) extends Term with MacroArg
  case class NumberWrapper(value: BigDecimal) extends Term with MacroArg
  case object NullLit extends Term

  sealed trait HighSymbol extends QueryNode with Term
  sealed trait TableSymbol extends QueryNode
  case class SymbolWrapper(label: String) extends Term with TableSymbol
  case class Raw(sql: String, args: List[Term]) extends Term with HighSymbol with TableSymbol with Cond
  case class MacroApply(symbol: String, args: Seq[MacroArg]) extends HighSymbol with TableSymbol with Cond
  case class Accessor(point: Int, resolvedSymbol: Option[SymbolWrapper] = None)
  case class SymbolWithAccessor(symbol: SymbolWrapper, accessor: Option[Accessor]) extends HighSymbol with MacroArg

  case class Op0(op: BinaryOp.Op) extends QueryNode
  case class Op1(op: LogicalOp.Op) extends QueryNode

  case class BCond0(lhs: Cond0, op: Op0, rhs: Cond0) extends Cond0
  case class BCond(lhs: Cond, op: Op1, rhs: Cond) extends Cond

  case class RBracket(cond: Cond) extends Term

  case class JoinType(value: JoinType.Op) extends QueryNode
  case class Join(joinType: JoinType, rhsTable: SymbolWrapper, on: Cond) extends QueryNode

  case class OrderType(value: OrderType.Op) extends QueryNode

  case class From(lhs: SymbolWrapper, rhss: List[Join]) extends QueryNode
  case class Select(values: List[HighSymbol]) extends QueryNode // Non Empty List
  case class Where(value: Cond) extends QueryNode
  case class LimitOffset(limit: NumberWrapper, offset: Option[NumberWrapper]) extends QueryNode // TODO ignore float number
  case class Order(orderType: OrderType, head: HighSymbol, tail: List[HighSymbol]) extends QueryNode

  case class Query(
    from: From,
    select: Option[Select],
    where: Option[Where],
    limitOffset: Option[LimitOffset],
    order: Option[Order])
      extends QueryNode

  object BinaryOp {
    sealed trait Op {
      val label: String
    }
    case object GT extends Op {
      val label = ">"
    }
    case object LT extends Op {
      val label = "<"
    }
    case object GE extends Op {
      val label = ">="
    }
    case object LE extends Op {
      val label = "<="
    }
    case object EQ extends Op {
      val label = "="
    }
    case object NE extends Op {
      val label = "<>"
    }
  }

  object LogicalOp {
    sealed trait Op {
      val label: String
    }
    case object And extends Op {
      val label = "&&"
    }
    case object Or extends Op {
      val label = "||"
    }
  }

  object JoinType {
    sealed trait Op {
      val label: String
    }
    case object LeftJoin extends Op {
      val label = "<<"
    }
    case object InnerJoin extends Op {
      val label = "><"
    }
  }

  object OrderType {
    sealed trait Op {
      val label: String
    }
    case object ASC extends Op {
      val label = "/>"
    }
    case object DESC extends Op {
      val label = "\\>"
    }
  }
}
