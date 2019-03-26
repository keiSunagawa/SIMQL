package me.kerfume.simql.smacro.func

import me.kerfume.simql.{ ASTMetaData, Result }
import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.QueryNode._
import me.kerfume.simql.smacro.MacroFunc
import me.kerfume.simql.parser.Parser

class HighSymbolMacro(
  val key: String,
  val parameters: Seq[MacroParam],
  retQuery: String // TODO generate from smacro interpreter
) extends MacroFunc with Checkable {
  type ReturnType = HighSymbol

  override def apply(args: Seq[MacroArg]): Result[HighSymbol] = {
    for {
      ret <- Parser.parse(Parser.highSymbol, retQuery).map(Right(_)).getOrElse(Left(s"function call error: parse failed return value. function name: $key"))
      varMap <- resolveArgs(args)
      visitor = new MacroArgsResolverVisitor(varMap)
      ok <- visitor.visitHighSymbol(ret).run(ASTMetaData.empty)
    } yield ok
  }
}
