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
  def term: Parser[Term] = nullLit | highSymbol | stringW | numberW | rbracket
  def rbracket: Parser[RBracket] = "(" ~> cond <~ ")" ^^ { RBracket(_) }

  def op0: Parser[Op0] = """(>=|<=|>|<|==|!=)""".r ^^ {
    case opStr =>
      val op = opStr match {
        case ">"  => BinaryOp.GT
        case "<"  => BinaryOp.LT
        case ">=" => BinaryOp.GE
        case "<=" => BinaryOp.LE
        case "==" => BinaryOp.EQ
        case "!=" => BinaryOp.NE
      }
      Op0(op)
  }

  def op1: Parser[Op1] = """(&&|\|\|)""".r ^^ {
    case opStr =>
      val op = opStr match {
        case "&&" => LogicalOp.And
        case "||" => LogicalOp.Or
      }
      Op1(op)
  }

  def cond0: Parser[Cond0] = term ~ rep(op0 ~ term) ^^ {
    case te ~ rep =>
      if (rep.nonEmpty) {
        val h :: t = rep.map { case op ~ rhs => op -> rhs }
        val first = BCond0(te, h._1, h._2)
        t.foldLeft(first) {
          case (acm, (op, rhs)) =>
            BCond0(acm, op, rhs)
        }
      } else {
        te
      }
  }

  def cond: Parser[Cond] = cond0 ~ rep(op1 ~ cond0) ^^ {
    case c0 ~ rep =>
      if (rep.nonEmpty) {
        val h :: t = rep.map { case op ~ rhs => op -> rhs }
        val first = BCond(c0, h._1, h._2)
        t.foldLeft(first) {
          case (acm, (op, rhs)) =>
            BCond(acm, op, rhs)
        }
      } else {
        c0
      }
  }

  def joinType = """<<|><""".r ^^ {
    case jtStr =>
      val jt = jtStr match {
        case "<<" => JoinType.LeftJoin
        case "><" => JoinType.InnerJoin
      }
      JoinType(jt)
  }
  def join = joinType ~ symbolW ~ "?>" ~ cond ^^ {
    case jt ~ rightTable ~ _ ~ c =>
      Join(jt, rightTable, c)
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
  def select = ":>" ~ highSymbol ~ rep(highSymbol) ^^ {
    case _ ~ col1 ~ cols =>
      Select(col1 +: cols)
  }
  def where = "?>" ~ cond ^^ {
    case _ ~ c =>
      Where(c)
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

  def macroArg: Parser[MacroArg] = cond | symbolWithAccessor | stringW | numberW
  def macroApply: Parser[MacroApply] =
    """\$[a-zA-Z][a-zA-Z0-9_]*\(""".r ~ opt(macroArg) ~ rep("," ~ macroArg) ~ ")" ^^ {
      case s ~ a1 ~ an ~ _ =>
        val symbol = s.tail.init
        val args = a1.toSeq ++ an.map { case _ ~ a => a }
        MacroApply(symbol, args)
    }
}
