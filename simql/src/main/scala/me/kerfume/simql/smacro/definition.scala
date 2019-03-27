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
}
