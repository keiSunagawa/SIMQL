package me.kerfume.simql.parser

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node.QueryNode._
import me.kerfume.simql.node.DefinitionNode._

trait CommonParser { self: JavaTokenParsers =>
  def string: Parser[String] = stringLiteral ^^ { s =>
    s.replaceAll("\"", "")
  }
  def number: Parser[BigDecimal] = decimalNumber ^^ { s =>
    BigDecimal(s)
  }
  def symbol: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
}

object Parser extends JavaTokenParsers with CommonParser with DefinitionParser with QueryParser {
  def parseSimql(code: String): Option[Query] = parse(simql, code) match {
    case Success(root, a) if (a.atEnd) => Some(root)
    // TODO dump error detail
    case Success(root, r) =>
      println(s"parsed: $root")
      println(s"parse failed. reason: ${r.first}")
      None
    case e =>
      println(s"parse failed. reason: $e")
      None
  }

  def parseDefinition(code: String): Option[DefinitionBlock] = parse(definitionBlock, code) match {
    case Success(root, a) if (a.atEnd) => Some(root)
    // TODO dump error detail
    case Success(root, r) =>
      println(s"parsed: $root")
      println(s"parse failed. reason: ${r.first}")
      None
    case e =>
      println(s"parse failed. reason: $e")
      None
  }
}
