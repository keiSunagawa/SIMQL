package me.kerfume.simql.parser

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node._

object Parser extends JavaTokenParsers with CoreParser with DefinitionParser with QueryParser {
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

  def parseDefinition(code: String): Option[List[UserFunction]] = parse(definitionBlock, code) match {
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
