package me.kerfume.simql

import Module._
import me.kerfume.simql.parser.Parser

object Develop {
  def run(query: String): Result[String] = simqlToMysql(query)
}

object MacroTest {
  def run() = {
    val a = DefinitionModule.loadPredef()
    println(a)
  }
}
