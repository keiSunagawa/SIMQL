package me.kerfume.simql

import me.kerfume.simql.node.QueryNode.Query
import me.kerfume.simql.node.DefinitionNode.{DefinitionBlock, MacroFunc}
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.resolver._
import me.kerfume.simql.generator.MySQLGenerator
import me.kerfume.simql.functions._
import cats.instances.list._

object Module {
  def simqlToMysql(query: String): Result[MySQLGenerator.Code] = {
    for {
      ast <- parseAndResolve(query)
      mysql = MySQLGenerator.generate(ast)
    } yield mysql
  }

  private[this] def parseAndResolve(query: String): Result[Query] = {
    for {
      ast <- Parser.parseSimql(query).toRight("failed parse.")
      meta <- makeMetadata(ast)
      resolved <- resolvers.foldLeft[Result[Query]](Right(ast)) {
                   case (before, resolver) =>
                     before match {
                       case Right(target) => resolver.resolve(target, meta)
                       case Left(e)       => Left(e)
                     }
                 }
    } yield resolved
  }

  private[this] def makeMetadata(ast: Query): Result[ASTMetaData] = {
    import me.kerfume.simql.smacro.func.{Generator => MGenerator}
    import smacro.MacroFunc.buildinInnerFunctions

    val analyzed = Analyzer.analyze(ast)
    for {
      predefMacro <- DefinitionModule.loadPredef()
      macroFuncs <- predefMacro.defs.collect { case c: MacroFunc => c }
                     .mapE(a => MGenerator.generate(a, buildinInnerFunctions.fmap))
    } yield
      analyzed.copy(
        macroFuncs = macroFuncs
      )
  }

  private[this] val resolvers: Seq[Resolver] = Seq(
    MacroFuncResolver,
    AccessorResolver
  )
}

object DefinitionModule {
  import scala.io.Source

  def loadPredef(): Result[DefinitionBlock] = {
    val code = Source.fromFile("predef.smql", "UTF-8").getLines.mkString("\n")
    parseBlock(code)
  }

  def parseBlock(code: String): Result[DefinitionBlock] = {
    Parser.parseDefinition(code).toRight("failed parse.")
  }
}
