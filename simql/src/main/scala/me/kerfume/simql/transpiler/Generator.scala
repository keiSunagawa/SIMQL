package me.kerfume.simql.transpiler

import me.kerfume.simql.node.SimqlNode._

trait Generator {
  type Code
  def generate(ast: SimqlRoot): Code
}

object MySQLGenerator extends Generator {
  type Code = String

  def generate(ast: SimqlRoot): Code = syntax.toSQL(ast)

  object syntax {
    def stringToSQL(node: StringWrapper): String = s"'${node.value}'"
    def numberToSQL(node: NumberWrapper): String = node.value.toString
    def nullToSQL(): String = "null"
    def rawToSQL(node: Raw): String = {
      val argsSQL = node.args.map(termToSQL)
      val sql = if (argsSQL.nonEmpty) node.sql.replaceAll("\\?", "%s").format(argsSQL: _*) else node.sql
      s"($sql)"
    }
    def symbolToSQL(node: SymbolWrapper): String = node.label
    def symbolWithAccessorToSQL(node: SymbolWithAccessor): String = {
      // この時点で未解決のシンボルはない前提...
      val accessorToken =
        node.accessor.map(a => a.resolvedSymbol.map(s => s"${symbolToSQL(s)}.").getOrElse("")).getOrElse("")
      s"${accessorToken}${symbolToSQL(node.symbol)}"
    }
    def highSymbolToSQL(node: HighSymbol): String = node match {
      case n: Raw                => rawToSQL(n)
      case n: SymbolWithAccessor => symbolWithAccessorToSQL(n)
      case n: MacroApply         => throw new RuntimeException(s"found unresolved macro. value: $n")
    }
    def termToSQL(node: Term): String = node match {
      case n: StringWrapper => stringToSQL(n)
      case n: NumberWrapper => numberToSQL(n)
      case NullLit          => nullToSQL()
      case n: SymbolWrapper => symbolToSQL(n)
      case n: HighSymbol    => highSymbolToSQL(n)
    }
    def binaryOpToSQL(node: BinaryOp): String = node.op.label
    def logicalOpToSQL(node: LogicalOp): String = node.op match {
      case LogicalOp.And => "AND"
      case LogicalOp.Or  => "OR"
    }
    def joinTypeToSQL(node: JoinType): String = node.value match {
      case JoinType.LeftJoin  => "LEFT JOIN"
      case JoinType.InnerJoin => "INNER JOIN"
    }
    def orderTypeToSQL(node: OrderType): String = node.value match {
      case OrderType.ASC  => "ASC"
      case OrderType.DESC => "DESC"
    }
    def condToSQL(node: Cond): String = node match {
      case BinaryCond(op, lhs, rhs) =>
        s"${highSymbolToSQL(lhs)} ${binaryOpToSQL(op)} ${termToSQL(rhs)}"
      case IsNull(lhs) =>
        s"${highSymbolToSQL(lhs)} IS NULL"
      case IsNotNull(lhs) =>
        s"${highSymbolToSQL(lhs)} IS NOT NULL"
      case n: MacroApply =>
        throw new RuntimeException(s"found unresolved macro. value: $n")
      case n: Raw => rawToSQL(n)
    }
    def exprRhsToSQL(node: ExprRhs): String = {
      s"${logicalOpToSQL(node.op)} ${condToSQL(node.value)}"
    }
    def exprToSQL(node: Expr): String = {
      s"${condToSQL(node.lhs)} ${node.rhss.map(exprRhsToSQL).mkString(" ")}"
    }
    def joinToSQL(node: Join): String = {
      s"${joinTypeToSQL(node.joinType)} ${symbolToSQL(node.rhsTable)} ON ${exprToSQL(node.on)}"
    }
    def fromToSQL(node: From): String = {
      s"${symbolToSQL(node.lhs)} ${node.rhss.map(joinToSQL).mkString(" ")}"
    }
    def selectToSQL(node: Select): String = {
      if (node.values.isEmpty) "*" else node.values.map(s => highSymbolToSQL(s)).mkString(", ")
    }
    def whereToSQL(node: Where): String = {
      exprToSQL(node.value)
    }
    def limitOffsetToSQL(node: LimitOffset): String = {
      val offsetSQL = node.offset.map(o => s"${numberToSQL(o)}, ").getOrElse("")
      s"$offsetSQL ${numberToSQL(node.limit)}" // mysql dialect
    }
    def orderToSQL(node: Order): String = {
      def pairToSQL(tpe: OrderType, symbol: HighSymbol): String = {
        s"${highSymbolToSQL(symbol)} ${orderTypeToSQL(tpe)}" // TODO
      }
      val tailSyntax = node.tail.map { t =>
        s", ${pairToSQL(node.orderType, t)}"
      }.mkString(" ")
      s"${pairToSQL(node.orderType, node.head)} $tailSyntax"
    }
    // TODO stack safe or @tailrec
    def toSQL(node: SimqlRoot): String = {
      val selectSQL = node.select.map(selectToSQL).getOrElse("*")
      val whereSQL = node.where.map(w => s"WHERE ${whereToSQL(w)}").getOrElse("")
      val limitOffsetSQL = node.limitOffset.map(l => s"LIMIT ${limitOffsetToSQL(l)}").getOrElse("")
      val orderSQL = node.order.map(o => s"ORDER BY ${orderToSQL(o)}").getOrElse("")
      s"SELECT $selectSQL FROM ${fromToSQL(node.from)} $whereSQL $limitOffsetSQL $orderSQL"
    }
  }
}
