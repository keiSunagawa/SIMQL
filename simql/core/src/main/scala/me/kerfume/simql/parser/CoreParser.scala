package me.kerfume.simql.parser

import me.kerfume.simql.node._
import scala.util.parsing.combinator.JavaTokenParsers

trait CoreParser { self: JavaTokenParsers =>
  def string: Parser[StringLit] = stringLiteral ^^ { s =>
    val value = s.replaceAll("\"", "")
    StringLit(value)
  }
  def number: Parser[NumberLit] = """-?(\d+(\.\d*)?|\d*\.\d+)""".r ^^ { s =>
    val value = BigDecimal(s)
    NumberLit(value)
  }
  def boolean: Parser[BooleanLit] = """(true|false)""".r ^^ {
    case "true"  => BooleanLit(true)
    case "false" => BooleanLit(false)
  }
  def symbol: Parser[SymbolLit] = """[a-zA-Z][a-zA-Z0-9_\.]*""".r ^^ { SymbolLit(_) }
}
