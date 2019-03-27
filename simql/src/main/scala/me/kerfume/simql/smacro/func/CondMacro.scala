package me.kerfume.simql.smacro.func

import me.kerfume.simql.{ASTMetaData, Result}
import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.QueryNode.{Cond, MacroArg}
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.smacro.MacroFunc
import me.kerfume.simql.functions._

class CondMacro(
  val key: String,
  val parameters: Seq[MacroParam],
  body: MacroFuncBody,
  fmap: Map[String, InnerFunc])
    extends MacroFunc
    with Checkable {
  type ReturnType = Cond

  override def apply(args: Seq[MacroArg]): Result[Cond] = {
    val statements :+ retQuery = body.body.toList
    for {
      varMap <- resolveArgs(args)
      varMapResolved = {
        qNodeToDNode(varMap) |>
          (statements.collect { case s: Bind => s }.foldLeft(_) { case (acm, b) => b.eval(acm, fmap) }) |>
          (dNodeToQNode(_))
      } // TODO rescue cond var
      ret <- returnQueryParse(retQuery, Parser.cond)
      visitor = new MacroArgsResolverVisitor(varMapResolved)
      ok <- visitor.visitCond(ret).run(ASTMetaData.empty)
    } yield ok
  }
}
