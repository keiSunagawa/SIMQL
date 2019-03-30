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

  def bind: Parser[Bind] = ("let" ~> symbol) ~ ("=" ~> expr) ^^ {
    case s ~ e =>
      Bind(s.label, e)
  }

  // def quasiquote: Parser[Quasiquote] = "q{" ~> """[^\}]*""".r <~ "}" ^^ { Quasiquote(_) } // いらないかも...

  def functionReturnNode: Parser[Expr] = expr // TODO

  // TODO parameter separate by ","
  def defun: Parser[SIMQLFunction] =
    ("defun" ~> symbol) ~ ("(" ~> rep(functionParam) <~ ")") ~ ("=>" ~> functionReturnType <~ "{") ~ (rep(bind) ~ functionReturnNode <~ "}") ^^ {
      case s ~ ps ~ retType ~ block =>
        val bd ~ retNode = block

        new SIMQLFunction {
          val key = s.label
          val params = ps
          val returnType = retType
          val body = bd
          val returnExpr = retNode
        }
    }

  def definitionBlock: Parser[List[SIMQLFunction]] = "define" ~> "{" ~> rep(defun) <~ "}"
}

object DefinitionParser extends JavaTokenParsers with DefinitionParser with CoreParser with QueryParser

object Test {
  import me.kerfume.simql._
  import me.kerfume.simql.defun.buildin

  val f1 =
    """defun f(num1: Number) => Number {
      |  let num2 = $add($num1(), 1)
      |  $num2()
      |}
    """.stripMargin
  val f1args = List(NumberLit(3))

  def run(): Result[Expr] = {
    val f = DefinitionParser.parse(DefinitionParser.defun, f1).get
    val scope = buildin.functions
    for {
      _ <- (new RefChecker).check(f, scope.map { case (key, _) => key -> () })
      _ <- (new TypeChecker).check(f, scope)
      res <- f(f1args, scope, ASTMetaData.empty)
    } yield res
  }
}
