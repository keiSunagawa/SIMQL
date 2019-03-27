package me.kerfume.simql.parser

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node.DefinitionNode._

trait DefinitionParser { self: JavaTokenParsers with CommonParser =>
  def macroParamType: Parser[MacroParamType] = "(String|Number|Symbol|Cond)".r ^^ {
    case "String" => StringType
    case "Number" => NumberType
    case "Symbol" => SymbolType
    case "Cond"   => ExprType
  }
  def macroParam: Parser[MacroParam] = symbol ~ ":" ~ macroParamType ^^ {
    case s ~ _ ~ tpe =>
      MacroParam(s, tpe)
  }

  def macroReturnType: Parser[MacroReturnType] = "(Cond|Symbol)".r ^^ {
    case "Symbol" => RetSymbol
    case "Cond"   => RetCond
  }

  def quasiquote: Parser[Quasiquote] = "q{" ~ """[^\}]*""".r ~ "}" ^^ {
    case _ ~ query ~ _ =>
      Quasiquote(query)
  }

  def macroStatement: Parser[MacroStatement] = quasiquote

  def macroFuncBody: Parser[MacroFuncBody] = rep(macroStatement) ^^ {
    case body =>
      MacroFuncBody(body)
  }

  def macroFunc: Parser[MacroFunc] =
    "defun" ~ symbol ~ "(" ~ rep(macroParam) ~ ")" ~ "=>" ~ macroReturnType ~ "{" ~ macroFuncBody ~ "}" ^^ {
      case _ ~ sym ~ _ ~ ps ~ _ ~ _ ~ retType ~ _ ~ body ~ _ =>
        MacroFunc(sym, ps, body, retType)
    }

  def definition: Parser[Definition] = macroFunc

  def definitionBlock: Parser[DefinitionBlock] = "define"~>"{"~>rep(definition)<~"}" ^^ { case defs => DefinitionBlock(defs) }
}
