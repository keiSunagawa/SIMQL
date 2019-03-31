package me.kerfume.simql.parser

import me.kerfume.simql.checker._

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node._

trait DefinitionParser { self: JavaTokenParsers with CoreParser with QueryParser =>
  def functionType: Parser[syntax.FType] = "(String|Number|Symbol|Expr)".r ^^ {
    case "String" => syntax.StringType
    case "Number" => syntax.NumberType
    case "Symbol" => syntax.SymbolType
    case "Expr"   => syntax.ExprType
  }
  def functionParam: Parser[FunctionParam] = (symbol <~ ":") ~ functionType ^^ {
    case s ~ tpe =>
      tpe match {
        case syntax.StringType => StringParam(s.label)
        case syntax.NumberType => NumberParam(s.label)
        case syntax.SymbolType => SymbolParam(s.label)
        case syntax.ExprType   => ExprParam(s.label)
      }
  }

  def functionReturnType: Parser[FunctionReturnType] = functionType ^^ {
    case syntax.StringType => StringType
    case syntax.NumberType => NumberType
    case syntax.SymbolType => SymbolType
    case syntax.ExprType   => ExprType
  }

  def dterm: Parser[Expr] = string | number | symbol | functionCall
  def queryBlock: Parser[Expr] = "q{" ~> expr <~ "}"
  def dexpr: Parser[Expr] =  queryBlock | dterm

  def bind: Parser[Bind] = ("let" ~> symbol) ~ ("=" ~> dexpr) ^^ {
    case s ~ e =>
      Bind(s.label, e)
  }

  def defun: Parser[SIMQLFunction] =
    ("defun" ~> symbol) ~ ("(" ~> opt(functionParam~rep(","~>functionParam)) <~ ")") ~ ("=>" ~> functionReturnType <~ "{") ~ (rep(bind) ~ dexpr <~ "}") ^^ {
      case s ~ ps ~ retType ~ block =>
        val bd ~ retNode = block
        val paramList = ps.toList.flatMap { case h~t => h :: t }

        new SIMQLFunction {
          val key = s.label
          val params = paramList
          val returnType = retType
          val body = bd
          val returnExpr = retNode
        }
    }

  def definitionBlock: Parser[List[SIMQLFunction]] = "define" ~> "{" ~> rep(defun) <~ "}"
}

object DefinitionParser extends JavaTokenParsers with DefinitionParser with CoreParser with QueryParser
