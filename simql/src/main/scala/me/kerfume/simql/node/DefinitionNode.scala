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
  case class MacroFunc(
    symbol: String,
    param: Seq[MacroParam],
    body: MacroFuncBody,
    retType: MacroReturnType)
      extends Definition

  case class DefinitionBlock(defs: List[Definition]) extends DefinitionNode
}
