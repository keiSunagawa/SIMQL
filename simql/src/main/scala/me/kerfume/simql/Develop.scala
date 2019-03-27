package me.kerfume.simql

import Module._
import me.kerfume.simql.parser.Parser

object Develop {
  def run(query: String): Either[String, String] = simqlToMysql(query)
}

object MacroTest {

  import me.kerfume.simql.smacro.func.Generator

  def run() = {
    import me.kerfume.simql.node.QueryNode._

    val a = DefinitionModule.loadPredef()
    println(a)
  }
}
