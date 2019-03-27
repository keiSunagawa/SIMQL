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

  def macroFunc: Parser[MacroFunc] =
    "defun" ~ symbol ~ "(" ~ rep(macroParam) ~ ")" ~ "=>" ~ macroReturnType ~ "{" ~ macroFuncBody ~ "}" ^^ {
      case _ ~ sym ~ _ ~ ps ~ _ ~ _ ~ retType ~ _ ~ body ~ _ =>
        MacroFunc(sym, ps, body, retType)
    }

  def definition: Parser[Definition] = macroFunc

  def definitionBlock: Parser[DefinitionBlock] = "define" ~> "{" ~> rep(definition) <~ "}" ^^ {
    case defs => DefinitionBlock(defs)
  }

  // macroFunc
  private[this] def stringW: Parser[StringWrapper] = string ^^ { StringWrapper(_) }
  private[this] def numberW: Parser[NumberWrapper] = number ^^ { NumberWrapper(_) }
  private[this] def symbolW: Parser[SymbolWrapper] = symbol ^^ { SymbolWrapper(_) }
  def value: Parser[Value] = stringW | numberW | symbolW

  def varSymbol: Parser[Var] = """\$[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { case v => Var(v.tail) }
  def funArg: Parser[FuncArg] = value | varSymbol
  def functionCall: Parser[FunctionCall] =
    """\$[a-zA-Z][a-zA-Z0-9_]*\(""".r ~ opt(funArg) ~ rep("," ~> funArg) <~ ")" ^^ {
      case s ~ a1 ~ an =>
        val symbol = s.tail.init
        val args = a1.toList ++ an
        FunctionCall(symbol, args)
    }

  def expr: Parser[Expr] = functionCall | value

  def bind: Parser[Bind] = "let" ~ symbol ~ "=" ~ expr ^^ {
    case _ ~ s ~ _ ~ e =>
      Bind(s, e)
  }

  def quasiquote: Parser[Quasiquote] = "q{" ~ """[^\}]*""".r ~ "}" ^^ {
    case _ ~ query ~ _ =>
      Quasiquote(query)
  }

  def macroStatement: Parser[MacroStatement] = quasiquote | bind

  def macroFuncBody: Parser[MacroFuncBody] = rep(macroStatement) ^^ {
    case body =>
      MacroFuncBody(body)
  }
}
