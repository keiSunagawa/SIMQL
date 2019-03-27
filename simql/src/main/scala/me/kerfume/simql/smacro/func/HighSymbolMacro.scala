package me.kerfume.simql.smacro.func

import me.kerfume.simql.{ASTMetaData, Result}
import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.QueryNode._
import me.kerfume.simql.smacro.MacroFunc
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.functions._

class HighSymbolMacro(
  val key: String,
  val parameters: Seq[MacroParam],
  body: MacroFuncBody,
  fmap: Map[String, InnerFunc])
    extends MacroFunc
    with Checkable {
  type ReturnType = HighSymbol

  override def apply(args: Seq[MacroArg]): Result[HighSymbol] = {
    val statements :+ retQuery = body.body.toList
    for {
      varMap <- resolveArgs(args)
      varMapResolved = {
        qNodeToDNode(varMap) |>
          (statements.collect { case s: Bind => s }.foldLeft(_) { case (acm, b) => b.eval(acm, fmap) }) |>
          (dNodeToQNode(_))
      } // TODO rescue cond var
      ret <- returnQueryParse[HighSymbol](retQuery, Parser.highSymbol)
      visitor = new MacroArgsResolverVisitor(varMapResolved)
      ok <- visitor.visitHighSymbol(ret).run(ASTMetaData.empty)
    } yield ok
  }
}
