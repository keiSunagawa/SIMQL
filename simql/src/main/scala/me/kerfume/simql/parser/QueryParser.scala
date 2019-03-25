package me.kerfume.simql.parser

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node.QueryNode._

trait QueryParser { self: JavaTokenParsers with CommonParser =>
  def stringW: Parser[StringWrapper] = string ^^ { StringWrapper(_) }
  def numberW: Parser[NumberWrapper] = number ^^ { NumberWrapper(_) }
  def symbolW: Parser[SymbolWrapper] = symbol ^^ { SymbolWrapper(_) }
  def nullLit: Parser[NullLit.type] = "null" ^^ { _ =>
    NullLit
  }

  def raw: Parser[Raw] = """\$`.*`""".r ~ opt("(" ~> rep(term) <~ ")") ^^ {
    case s ~ as =>
      val args = as.toList.flatten
      val sql = s.tail.replaceAll("`", "")
      Raw(sql, args)
  }
  def accessor: Parser[Accessor] = """\$[0-9]""".r ^^ { case s => Accessor(s.tail.toInt) }
  def symbolWithAccessor: Parser[SymbolWithAccessor] = opt(accessor ~ ".") ~ symbolW ^^ {
    case acOpt ~ s =>
      val accessor = acOpt.map { case ac ~ _ => ac }
      SymbolWithAccessor(s, accessor)
  }
  def highSymbol: Parser[HighSymbol] = macroApply | raw | symbolWithAccessor
  def term: Parser[Term] = nullLit | highSymbol | stringW | numberW

  def binaryOp: Parser[BinaryOp] = """(>=|<=|>|<|==|!=|in)""".r ^^ {
    case opStr =>
      val op = opStr match {
        case ">"  => BinaryOp.GT
        case "<"  => BinaryOp.LT
        case ">=" => BinaryOp.GE
        case "<=" => BinaryOp.LE
        case "==" => BinaryOp.EQ
        case "!=" => BinaryOp.NE
      }
      BinaryOp(op)
  }
  def binaryCond = highSymbol ~ binaryOp ~ term ^^ { case lhs ~ op ~ rhs => BinaryCond(op, lhs, rhs) }
  def cond: Parser[Cond] = binaryCond | macroApply | raw

  def logicalOp = """(&&|\|\|)""".r ^^ {
    case opStr =>
      val op = opStr match {
        case "&&" => LogicalOp.And
        case "||" => LogicalOp.Or
      }
      LogicalOp(op)
  }

  def expr: Parser[Expr] = cond ~ rep(logicalOp ~ cond) ^^ {
    case b ~ body =>
      val rhss = body.map { case lo ~ bc => ExprRhs(lo, bc) }
      Expr(b, rhss)
  }

  def joinType = """<<|><""".r ^^ {
    case jtStr =>
      val jt = jtStr match {
        case "<<" => JoinType.LeftJoin
        case "><" => JoinType.InnerJoin
      }
      JoinType(jt)
  }
  def join = joinType ~ symbolW ~ "?" ~ expr ^^ {
    case jt ~ rightTable ~ _ ~ exp =>
      Join(jt, rightTable, exp)
  }

  def orderType = """/>|\\>""".r ^^ {
    case opStr =>
      val op = opStr match {
        case "/>"  => OrderType.ASC
        case "\\>" => OrderType.DESC
      }
      OrderType(op)
  }

  def from = symbolW ~ rep(join) ^^ {
    case table ~ joins =>
      From(table, joins)
  }
  def select = ":" ~ highSymbol ~ rep(highSymbol) ^^ {
    case _ ~ col1 ~ cols =>
      Select(col1 +: cols)
  }
  def where = "?" ~ expr ^^ {
    case _ ~ exp =>
      Where(exp)
  }
  def limitOffset = "@" ~ numberW ~ opt("-" ~ numberW) ^^ {
    case _ ~ limit ~ offsetSyntax =>
      val offset = offsetSyntax.map { case _ ~ ofs => ofs }
      LimitOffset(limit, offset)
  }
  def order = orderType ~ highSymbol ~ rep(highSymbol) ^^ {
    case tpe ~ col1 ~ coln =>
      Order(tpe, col1, coln)
  }

  def simql = from ~ opt(select) ~ opt(where) ~ opt(limitOffset) ~ opt(order) ^^ {
    case f ~ s ~ w ~ l ~ o =>
      Query(f, s, w, l, o)
  }

  def macroArg: Parser[MacroArg] = expr | symbolWithAccessor | stringW | numberW
  def macroApply: Parser[MacroApply] =
    """\$[a-zA-Z][a-zA-Z0-9_]*\(""".r ~ opt(macroArg) ~ rep("," ~ macroArg) ~ ")" ^^ {
      case s ~ a1 ~ an ~ _ =>
        val symbol = s.tail.init
        val args = a1.toSeq ++ an.map { case _ ~ a => a }
        MacroApply(symbol, args)
    }
}
