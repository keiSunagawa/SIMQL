package me.kerfume.simql.smacro

import me.kerfume.simql._
import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.QueryNode._
import me.kerfume.simql.smacro.func._

trait MacroFunc {
  val key: String
  type ReturnType

  val parameters: Seq[MacroParam]

  def apply(args: Seq[MacroArg]): Result[ReturnType]
}

object MacroFunc {
  abstract class TermMacro extends MacroFunc {
    type ReturnType = Term
  }

  implicit class FuncsOps(fs: Seq[MacroFunc]) {
    def highSymbolMacros: Map[String, HighSymbolMacro] =
      fs.collect {
        case m: HighSymbolMacro =>
          m.key -> m
      }(collection.breakOut)
    def termMacros: Map[String, TermMacro] =
      fs.collect {
        case m: TermMacro =>
          m.key -> m
      }(collection.breakOut)
    def condMacros: Map[String, CondMacro] =
      fs.collect {
        case m: CondMacro =>
          m.key -> m
      }(collection.breakOut)
  }

  object buildin {
    import me.kerfume.simql.parser.Parser

    val countDefine: String = """defun c(col: Symbol) => Symbol {
                                |  q{ $`COUNT(?)`($col()) }
                                |}""".stripMargin

    val cAst = Parser.parse(Parser.macroFunc, countDefine).get
    val Count = (for {
      f <- Generator.generate(cAst)
    } yield f).right.get

    val likeDefine: String = """defun like(col: Symbol str: String) => Cond {
                               |  q{ $`? LIKE(?)`($col() $str()) }
                               |}""".stripMargin

    val likeAst = Parser.parse(Parser.macroFunc, likeDefine).get
    val Like = (for {
      f <- Generator.generate(likeAst)
    } yield f).right.get

    //  case object JoinExample extends CondMacro {
    //    val key = "je"
    //    def apply0(
    //      symbolArgs: Seq[SymbolWithAccessor],
    //      stringArgs: Seq[StringWrapper],
    //      numberArgs: Seq[NumberWrapper]
    //    ): Result[Cond] = {
    //      // ignore args
    //      Right(BinaryCond(BinaryOp(BinaryOp.EQ), SymbolWithAccessor(SymbolWrapper("id"), Some(Accessor(0))), SymbolWithAccessor(SymbolWrapper("author_id"), Some(Accessor(1)))))
    //    }
    //  }
  }
}
