package me.kerfume.simql.parser

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node._
import me.kerfume.simql.node.SIMQLFunction._

trait DefinitionParser { self: JavaTokenParsers with CoreParser with QueryParser =>
  def functionType: Parser[SIMQLType] = "(String|Number|Boolean|Symbol|Expr|Raw)".r ^^ {
    case "String"  => StringType
    case "Number"  => NumberType
    case "Boolean" => BooleanType
    case "Symbol"  => SymbolType
    case "Raw"     => RawType
    case "Expr"    => ExprType
  }
  def functionParam: Parser[FunctionParam] = (symbol <~ ":") ~ functionType ^^ {
    case s ~ tpe => FunctionParam(s.label, tpe)
  }

  def dterm: Parser[Expr] = string | number | symbol | functionCall
  def queryBlock: Parser[Expr] = "q{" ~> expr <~ "}"
  def dexpr: Parser[Expr] = queryBlock | dterm

  def bind: Parser[Bind] = ("let" ~> symbol) ~ ("=" ~> dexpr) ^^ {
    case s ~ e =>
      Bind(s.label, e)
  }

  def defun: Parser[UserFunction] = // TODO paramは一つ以上
    ("defun" ~> symbol) ~ ("(" ~> opt(functionParam ~ rep("," ~> functionParam)) <~ ")") ~ ("=>" ~> functionType <~ "{") ~ (rep(
      bind
    ) ~ dexpr <~ "}") ^^ {
      case s ~ ps ~ retType ~ block =>
        val bd ~ retNode = block
        val pList = ps.toList.flatMap { case h ~ t => h :: t }
        val plast = pList.last
        val pInit = pList.init

        val last = Closure(key = s.label, param = plast, returnType = retType, body = bd, returnValue = retNode)

        pInit.foldRight(last) {
          case (p, next) =>
            val nextType = FunctionType(next.param.tpe, next.returnType)
            Closure(key = s.label, param = p, returnType = nextType, body = Nil, returnValue = next)
        }
    }

  def definitionBlock: Parser[List[UserFunction]] = "define" ~> "{" ~> rep(defun) <~ "}"
}

object DefinitionParser extends JavaTokenParsers with DefinitionParser with CoreParser with QueryParser
