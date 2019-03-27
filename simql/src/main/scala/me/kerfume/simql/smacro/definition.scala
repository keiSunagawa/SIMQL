package me.kerfume.simql.smacro

import me.kerfume.simql._
import me.kerfume.simql.node.DefinitionNode.{NumberWrapper => DNumber, _}
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

  object buildinInnerFunctions {
    val fmap: Map[String, InnerFunc] = Seq(Add).map(f => f.symbol -> f).toMap
    object Add extends InnerFunc {
      val symbol: String = "add"
      protected[this] val param: Seq[(String, InnerFuncParam)] = ("num1" -> NumberP) :: ("num2" -> NumberP) :: Nil
      protected[this] val body: Seq[Bind] = Nil
      override protected[this] def eval0(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value = {
        // 一応引数値がscopeに存在することは保証されている...が...
        val v = param.map {
          case (key, _) =>
            scope(key).asInstanceOf[DNumber].value
        }.sum
        DNumber(v)
      }
    }
  }
}
