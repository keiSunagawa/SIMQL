package me.kerfume.simql.smacro.func

import me.kerfume.simql.{ ASTMetaData, Result }
import me.kerfume.simql.node.DefinitionNode.MacroParam
import me.kerfume.simql.node.QueryNode.{ Cond, MacroArg }
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.smacro.MacroFunc

class CondMacro (
  val key: String,
  val parameters: Seq[MacroParam],
  retQuery: String // TODO generate from smacro interpreter
) extends MacroFunc with Checkable {
  type ReturnType = Cond

  override def apply(args: Seq[MacroArg]): Result[Cond] = {
    for {
      ret <- Parser.parse(Parser.cond, retQuery).map(Right(_)).getOrElse(Left(s"function call error: parse failed return value. function name: $key"))
      varMap <- resolveArgs(args)
      visitor = new MacroArgsResolverVisitor(varMap)
      ok <- visitor.visitCond(ret).run(ASTMetaData.empty)
    } yield ok
  }
}
