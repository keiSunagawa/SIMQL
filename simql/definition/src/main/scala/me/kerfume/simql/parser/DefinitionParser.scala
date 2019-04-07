package me.kerfume.simql.parser

import scala.util.parsing.combinator.JavaTokenParsers
import me.kerfume.simql.node._
import me.kerfume.simql.node.SIMQLFunction._

trait DefinitionParser { self: JavaTokenParsers with CoreParser with QueryParser =>
  def lowerType: Parser[SIMQLType] = "(String|Number|Boolean|Symbol|Expr|Raw)".r ^^ {
    case "String"  => StringType
    case "Number"  => NumberType
    case "Boolean" => BooleanType
    case "Symbol"  => SymbolType
    case "Raw"     => RawType
    case "Expr"    => ExprType
  }
  def listType: Parser[ListType] = "List<" ~> simqlType <~ ">" ^^ { ListType(_) }
  def atomicType: Parser[SIMQLType] = lowerType | listType | ("(" ~> functionType <~ ")")
  def functionType: Parser[SIMQLType] = (atomicType <~ "=>") ~ atomicType ~ rep("=>" ~> atomicType) ^^ {
    case f ~ t ~ nexts =>
      if (nexts.nonEmpty) {
        val last = nexts.last
        val types = f :: t :: nexts.init
        types.foldRight[SIMQLType](last) {
          case (prm, ret) =>
            FunctionType(prm, ret)
        }
      } else FunctionType(f, t)
  }
  def simqlType: Parser[SIMQLType] = functionType | atomicType

  def functionParam: Parser[FunctionParam] = (symbol <~ ":") ~ simqlType ^^ {
    case s ~ tpe => FunctionParam(s.label, tpe)
  }

  def nil: Parser[SIMQLList] = "nil<" ~> simqlType <~ ">" ^^ { SIMQLList(Nil, _) }

  def dterm: Parser[Expr] = string | nil | number | symbol | functionCall
  def queryBlock: Parser[Expr] = "q{" ~> expr <~ "}"
  def dexpr: Parser[Expr] = queryBlock | dterm | closure

  def bind: Parser[Bind] = ("let" ~> symbol) ~ ("=" ~> dexpr) ^^ {
    case s ~ e =>
      Bind(s.label, e)
  }

  def function =
    ("(" ~> functionParam ~ rep("," ~> functionParam) <~ ")") ~ ("=>" ~> simqlType <~ "{") ~ (rep(bind) ~ dexpr <~ "}")
  def closure: Parser[Closure] = function ^^ {
    case ps ~ retType ~ block =>
      val bd ~ retNode = block
      val pList = {
        val ph ~ pt = ps
        ph :: pt
      }
      val plast = pList.last
      val pInit = pList.init

      val last = Closure(param = plast, returnType = retType, body = bd, returnValue = retNode)

      pInit.foldRight(last) {
        case (p, next) =>
          val nextType = FunctionType(next.param.tpe, next.returnType)
          Closure(param = p, returnType = nextType, body = Nil, returnValue = next)
      }
  }

  def defun: Parser[UserFunction] =
    ("defun" ~> symbol) ~ function ^^ {
      case s ~ f =>
        val ps ~ retType ~ block = f
        val bd ~ retNode = block
        val pList = {
          val ph ~ pt = ps
          ph :: pt
        }
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
