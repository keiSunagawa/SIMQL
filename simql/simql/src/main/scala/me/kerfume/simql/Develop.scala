package me.kerfume.simql

import Module._
import me.kerfume.simql.parser.Parser
import scala.io.Source

object Develop {
  val predef = Source.fromFile("predef.smql", "UTF-8").getLines.mkString("\n")
  val userdef = Source.fromFile("userdef.smql", "UTF-8").getLines.mkString("\n")
  def run(query: String): Result[String] = simqlToMysql(query, Some(predef), Some(userdef))
}
