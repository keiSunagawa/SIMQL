package me.kerfume.simql.parser

import cats.data.NonEmptyList
import me.kerfume.simql.node._

import scala.util.parsing.combinator.JavaTokenParsers

trait QueryParser { self: JavaTokenParsers with CoreParser =>
  def nullLit: Parser[NullLit.type] = "null" ^^ { _ =>
    NullLit
  }

  def raw: Parser[Raw] = """\$`.*`""".r ~ opt("(" ~> rep(term) <~ ")") ^^ {
    case s ~ as =>
      val args = as.toList.flatten
      val sql = s.tail.replaceAll("`", "")
      Raw(sql, args)
  }

  def column: Parser[Expr] = opt("""\$[0-9]""".r <~ ".") ~ symbol ^^ {
    case accessor ~ s =>
      accessor match {
        case Some(ac) => FunctionCall("table_access", List(s, NumberLit(ac.tail.toInt)))
        case None     => s
      }
  }
  def term: Parser[Expr] = nullLit | string | number | column | rbracket | raw | functionCall
  def rbracket: Parser[RBracket] = "(" ~> expr <~ ")" ^^ { RBracket(_) }

  def op0: Parser[Op] = """(>=|<=|>|<|==|!=)""".r ^^ { opStr =>
    val op = opStr match {
      case ">"  => ExprOp.GT
      case "<"  => ExprOp.LT
      case ">=" => ExprOp.GE
      case "<=" => ExprOp.LE
      case "==" => ExprOp.EQ
      case "!=" => ExprOp.NE
    }
    Op(op)
  }

  def op1: Parser[Op] = """(&&|\|\|)""".r ^^ { opStr =>
    val op = opStr match {
      case "&&" => ExprOp.And
      case "||" => ExprOp.Or
    }
    Op(op)
  }

  def expr0: Parser[Expr] = term ~ rep(op0 ~ term) ^^ {
    case te ~ rep =>
      if (rep.nonEmpty) {
        val h :: t = rep.map { case op ~ rhs => op -> rhs }
        val first = BExpr(te, h._1, h._2)
        t.foldLeft(first) {
          case (acm, (op, rhs)) =>
            BExpr(acm, op, rhs)
        }
      } else {
        te
      }
  }

  def expr: Parser[Expr] = expr0 ~ rep(op1 ~ expr0) ^^ {
    case e0 ~ rep =>
      if (rep.nonEmpty) {
        val h :: t = rep.map { case op ~ rhs => op -> rhs }
        val first = BExpr(e0, h._1, h._2)
        t.foldLeft(first) {
          case (acm, (op, rhs)) =>
            BExpr(acm, op, rhs)
        }
      } else {
        e0
      }
  }

  def joinType = """<<|><""".r ^^ { jtStr =>
    val jt = jtStr match {
      case "<<" => JoinType.LeftJoin
      case "><" => JoinType.InnerJoin
    }
    JoinType(jt)
  }
  def join = joinType ~ term ~ "?>" ~ expr ^^ {
    case jt ~ rightTable ~ _ ~ c =>
      Join(jt, rightTable, c)
  }

  def orderType = """/>|\\>""".r ^^ { opStr =>
    val op = opStr match {
      case "/>"  => OrderType.Asc
      case "\\>" => OrderType.Desc
    }
    OrderType(op)
  }

  def from = expr ~ rep(join) ^^ {
    case table ~ joins =>
      From(table, joins)
  }
  def select = ":>" ~> term ~ rep(term) ^^ {
    case col1 ~ cols =>
      Select(NonEmptyList.of(col1, cols: _*))
  }
  def where = "?>" ~> expr ^^ { Where(_) }

  def limitOffset = "@" ~> number ~ opt("-" ~> number) ^^ {
    case limit ~ offset =>
      LimitOffset(limit, offset)
  }
  def order = orderType ~ term ~ rep(term) ^^ {
    case tpe ~ col1 ~ cols =>
      Order(tpe, NonEmptyList.of(col1, cols: _*))
  }

  def simql = from ~ opt(select) ~ opt(where) ~ opt(limitOffset) ~ opt(order) ^^ {
    case f ~ s ~ w ~ l ~ o =>
      Query(f, s, w, l, o)
  }

  def functionCall: Parser[FunctionCall] =
    """\$[a-zA-Z][a-zA-Z0-9_]*""".r ~ opt("(" ~> (opt(expr ~ rep("," ~> expr))) <~ ")") ^^ {
      case s ~ as =>
        val symbol = s.tail
        val args = as.flatten.toList.flatMap { case h ~ t => h :: t }
        FunctionCall(symbol, args)
    }
}

object QueryParser extends JavaTokenParsers with QueryParser with CoreParser
