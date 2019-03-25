package me.kerfume.simql.node

sealed trait SimqlNode

object SimqlNode {
  sealed trait Term extends SimqlNode
  case class StringWrapper(value: String) extends Term with MacroArg
  case class NumberWrapper(value: BigDecimal) extends Term with MacroArg
  case object NullLit extends Term

  sealed trait HighSymbol extends SimqlNode with Term
  sealed trait TableSymbol extends SimqlNode
  sealed trait MacroArg extends SimqlNode
  case class SymbolWrapper(label: String) extends Term with TableSymbol
  case class Raw(sql: String, args: List[Term]) extends Term with HighSymbol with TableSymbol with Cond
  case class MacroApply(symbol: String, args: Seq[MacroArg]) extends HighSymbol with TableSymbol with Cond
  case class Accessor(point: Int, resolvedSymbol: Option[SymbolWrapper] = None)
  case class SymbolWithAccessor(symbol: SymbolWrapper, accessor: Option[Accessor]) extends HighSymbol with MacroArg

  case class BinaryOp(op: BinaryOp.Op) extends SimqlNode
  sealed trait Cond extends SimqlNode
  case class IsNull(lhs: HighSymbol) extends Cond // only generate by ast visitor
  case class IsNotNull(lhs: HighSymbol) extends Cond // only generate by ast visitor
  case class BinaryCond(op: BinaryOp, lhs: HighSymbol, rhs: Term) extends Cond

  case class LogicalOp(op: LogicalOp.Op) extends SimqlNode
  case class ExprRhs(op: LogicalOp, value: Cond) extends SimqlNode
  case class Expr(lhs: Cond, rhss: List[ExprRhs]) extends SimqlNode with MacroArg

  case class JoinType(value: JoinType.Op) extends SimqlNode
  case class Join(joinType: JoinType, rhsTable: SymbolWrapper, on: Expr) extends SimqlNode

  case class OrderType(value: OrderType.Op) extends SimqlNode

  case class From(lhs: SymbolWrapper, rhss: List[Join]) extends SimqlNode
  case class Select(values: List[HighSymbol]) extends SimqlNode // Non Empty List
  case class Where(value: Expr) extends SimqlNode
  case class LimitOffset(limit: NumberWrapper, offset: Option[NumberWrapper]) extends SimqlNode // TODO ignore float number
  case class Order(orderType: OrderType, head: HighSymbol, tail: List[HighSymbol]) extends SimqlNode

  case class SimqlRoot(
    from: From,
    select: Option[Select],
    where: Option[Where],
    limitOffset: Option[LimitOffset],
    order: Option[Order])
    extends SimqlNode

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
    // TODO 扱いにこまる...
    // case object IN extends Op {
    //   val label = "in"
    // }
    // case object LIKE extends Op {
    //   val label = "like"
    // }
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
