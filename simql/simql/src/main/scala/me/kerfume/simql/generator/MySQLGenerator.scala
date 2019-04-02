package me.kerfume.simql.generator

import me.kerfume.simql.node._

object MySQLGenerator extends Generator {
  type Code = String

  def generate(ast: Query): Code = syntax.toSQL(ast)

  object syntax {
    def stringToSQL(node: StringLit): String = s"'${node.value}'"
    def numberToSQL(node: NumberLit): String = node.value.toString
    def nullToSQL(): String = "null"
    def rawToSQL(node: Raw): String = {
      val argsSQL = node.args.map(exprToSQL)
      val sql = if (argsSQL.nonEmpty) node.sql.replaceAll("\\?", "%s").format(argsSQL: _*) else node.sql
      s"($sql)" // TODO do not might be need round bracket?
    }
    def symbolToSQL(node: SymbolLit): String = node.label

    def exprToSQL(node: Expr): String = node match {
      case BExpr(lhs, op, rhs) => s"(${exprToSQL(lhs)}) ${opToSQL(op)} (${exprToSQL(rhs)})"
      case n: Raw              => rawToSQL(n)
      case RBracket(expr)      => s"(${exprToSQL(expr)})"
      case n: StringLit        => stringToSQL(n)
      case n: NumberLit        => numberToSQL(n)
      case n: SymbolLit        => symbolToSQL(n)
      case NullLit             => nullToSQL()
      case f: Call             => throw new RuntimeException(s"unresolve function call found. ${f.symbol}")
    }

    def opToSQL(node: Op): String = node.op match {
      case ExprOp.And => "AND"
      case ExprOp.Or  => "OR"
      case ExprOp.EQ  => "="
      case ExprOp.NE  => "<>"
      case ExprOp.GT  => "<"
      case ExprOp.LT  => ">"
      case ExprOp.GE  => "<="
      case ExprOp.LE  => ">="
    }

    def joinTypeToSQL(node: JoinType): String = node.value match {
      case JoinType.LeftJoin  => "LEFT JOIN"
      case JoinType.InnerJoin => "INNER JOIN"
    }
    def orderTypeToSQL(node: OrderType): String = node.value match {
      case OrderType.Asc  => "ASC"
      case OrderType.Desc => "DESC"
    }

    def joinToSQL(node: Join): String = {
      s"${joinTypeToSQL(node.joinType)} ${exprToSQL(node.rhsTable)} ON ${exprToSQL(node.on)}"
    }
    def fromToSQL(node: From): String = {
      s"${exprToSQL(node.lhs)} ${node.rhss.map(joinToSQL).mkString(" ")}"
    }
    def selectToSQL(node: Select): String = {
      node.columns.map(s => exprToSQL(s)).toList.mkString(", ")
    }
    def whereToSQL(node: Where): String = {
      exprToSQL(node.expr)
    }
    def limitOffsetToSQL(node: LimitOffset): String = {
      val offsetSQL = node.offset.map(o => s"${numberToSQL(o)}, ").getOrElse("")
      s"$offsetSQL ${numberToSQL(node.limit)}" // mysql dialect
    }
    def orderToSQL(node: Order): String = {
      def pairToSQL(tpe: OrderType, symbol: Expr): String = {
        s"${exprToSQL(symbol)} ${orderTypeToSQL(tpe)}"
      }
      node.columns.map(pairToSQL(node.orderType, _)).toList.mkString(", ")
    }

    def toSQL(node: Query): String = {
      val selectSQL = node.select.map(selectToSQL).getOrElse("*")
      val whereSQL = node.where.map(w => s"WHERE ${whereToSQL(w)}").getOrElse("")
      val limitOffsetSQL = node.limitOffset.map(l => s"LIMIT ${limitOffsetToSQL(l)}").getOrElse("")
      val orderSQL = node.order.map(o => s"ORDER BY ${orderToSQL(o)}").getOrElse("")
      s"SELECT $selectSQL FROM ${fromToSQL(node.from)} $whereSQL $limitOffsetSQL $orderSQL"
    }
  }
}
