package me.kerfume.simql.node

sealed trait DefinitionNode

object DefinitionNode {
  sealed trait MacroParamType extends DefinitionNode
  case object StringType extends MacroParamType
  case object NumberType extends MacroParamType
  case object SymbolType extends MacroParamType
  case object ExprType extends MacroParamType
  case class MacroParam(symbol: String, tpe: MacroParamType) extends MacroParamType

  sealed trait MacroReturnType extends DefinitionNode
  case object RetSymbol extends MacroReturnType
  case object RetCond extends MacroReturnType

  sealed trait MacroStatement extends DefinitionNode
  case class Quasiquote(query: String) extends MacroStatement
  case class MacroFuncBody(body: Seq[MacroStatement]) // last statement required quasiquote

  sealed trait Definition extends DefinitionNode

  case class DefinitionBlock(defs: List[Definition]) extends DefinitionNode

  sealed trait FunctionSyntax extends DefinitionNode
  sealed trait Expr extends FunctionSyntax {
    def eval(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value
  }
  sealed trait FuncArg extends FunctionSyntax
  sealed trait Value extends FuncArg with Expr
  case class StringWrapper(value: String) extends Value {
    def eval(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value = this
  }
  case class NumberWrapper(value: BigDecimal) extends Value {
    def eval(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value = this
  }
  case class SymbolWrapper(label: String) extends Value {
    def eval(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value = this
  }
  case class Var(symbol: String) extends FuncArg

  case class FunctionCall(symbol: String, args: List[FuncArg]) extends Expr {
    def eval(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value = {
      val f = fscope.get(symbol).getOrElse(throw new RuntimeException("error. function not found."))
      f.eval(args, scope, fscope)
    }
  }

  case class Bind(symbol: String, value: Expr) extends FunctionSyntax with MacroStatement {
    def eval(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Map[String, Value] = {
      val ret = value.eval(scope, fscope)
      scope + (symbol -> ret)
    }
  }

  // ひとまずユーザ定義できない特殊な関数classを定義しておく
  sealed trait InnerFuncParam
  case object StringP extends InnerFuncParam
  case object NumberP extends InnerFuncParam
  case object SymbolP extends InnerFuncParam
  trait InnerFunc extends FunctionSyntax {
    val symbol: String
    protected[this] val param: Seq[(String, InnerFuncParam)]
    protected[this] val body: Seq[Bind]

    private[this] def typeCheck(param: InnerFuncParam, arg: Value): Boolean = (param, arg) match {
      case (StringP, _: StringWrapper) => true
      case (NumberP, _: NumberWrapper) => true
      case (SymbolP, _: SymbolWrapper) => true
      case _                           => false
    }

    protected[this] def eval0(scope: Map[String, Value], fscope: Map[String, InnerFunc]): Value

    def eval(args: Seq[FuncArg], outerScope: Map[String, Value], fscope: Map[String, InnerFunc]): Value = {
      // args check and create scope
      val scope = param
        .zip(args)
        .map {
          case ((key, tpe), arg) =>
            val value: Value = (arg match {
              case v: Value => if (typeCheck(tpe, v)) Some(v) else None
              case vr: Var  => outerScope.get(vr.symbol).filter(typeCheck(tpe, _))
            }).getOrElse(throw new RuntimeException("function call type mismatch."))
            key -> value
        }
        .toMap
      val resolved = body.foldLeft(scope) { case (s, b) => b.eval(s, fscope) }
      eval0(resolved, fscope)
    }
  }

  // ユーザ側で定義した関数
  case class MacroFunc(
    symbol: String,
    param: Seq[MacroParam],
    body: MacroFuncBody,
    retType: MacroReturnType)
      extends Definition
}
