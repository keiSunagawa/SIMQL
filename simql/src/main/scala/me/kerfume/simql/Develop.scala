package me.kerfume.simql

import me.kerfume.simql.transpiler.Module._
import me.kerfume.simql.transpiler.parser.Parser

object Develop {
  def run(query: String): Either[String, String] = simqlToMysql(query)
}

object MacroTest {
  import  me.kerfume.simql.transpiler.querymacro.MacroFuncGenerator

  def run(qq: String, sym: String) = {
    import me.kerfume.simql.node.SimqlNode._

    val in = s"""defun f(a: Symbol) => Symbol = {
                |  ${qq}
                |}
              """.stripMargin
    val ast = Parser.parse(Parser.macroFunc, in).get
    val args = Seq(SymbolWithAccessor(SymbolWrapper(sym), None))
    MacroFuncGenerator.generate(ast).right.get.apply(args)
  }
}
