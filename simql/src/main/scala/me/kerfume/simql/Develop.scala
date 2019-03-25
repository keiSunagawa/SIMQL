package me.kerfume.simql

import Module._
import me.kerfume.simql.parser.Parser

object Develop {
  def run(query: String): Either[String, String] = simqlToMysql(query)
}

object MacroTest {

  import me.kerfume.simql.smacro.func.MacroFuncGenerator

  def run(qq: String, sym: String) = {
    import me.kerfume.simql.node.QueryNode._

    val in = s"""defun f(a: Symbol) => Symbol = {
                |  ${qq}
                |}
              """.stripMargin
    val ast = Parser.parse(Parser.macroFunc, in).get
    val args = Seq(SymbolWithAccessor(SymbolWrapper(sym), None))
    MacroFuncGenerator.generate(ast).right.get.apply(args)
  }
}
