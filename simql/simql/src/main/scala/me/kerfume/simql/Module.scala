package me.kerfume.simql

import me.kerfume.simql.node._
import me.kerfume.simql.analyzer._
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.resolver._
import me.kerfume.simql.generator.MySQLGenerator
import me.kerfume.simql.functions._
import cats.instances.list._
import me.kerfume.simql.checker._

object Module {
  private[this] val analyzer = new TableAnalyzer

  def simqlToMysql(query: String): Result[MySQLGenerator.Code] = {
    for {
      ast <- parseAndResolve(query)
      mysql = MySQLGenerator.generate(ast)
    } yield mysql
  }

  private[this] def parseAndResolve(query: String): Result[Query] = {
    for {
      ast <- Parser.parseSimql(query).toRight(UnhandleError("failed parse."))
      meta <- makeContext(ast)
      resolved <- resolvers.foldE(ast) { case (before, resolver) => resolver.resolve(before, meta) }
      _ <- queryChecker.mapE(_.check(resolved, meta))
    } yield resolved
  }

  private[this] def makeContext(ast: Query): Result[QueryContext] = {
    val ctx = QueryContext.empty

    val analyzed = analyzer.analyze(ast, ctx)
    for {
      predef <- DefinitionModule.loadPredef()
      userdef <- DefinitionModule.loadUserdef()
      buildin = me.kerfume.simql.defun.buildin.functions
      gscope <- (predef ++ userdef).foldE(buildin) {
                 case (s, f) =>
                   for {
                     _ <- (new TypeChecker).check(f, s)
                   } yield s + (f.key -> Pure(f))
               }
    } yield
      analyzed.copy(
        globalScope = gscope
      )
  }

  private[this] val resolvers: List[Resolver] = List(
    new FunctionResolver,
    new NullResolver,
    new FunctionResolver // FIXME function double for null resolver...
  )
  private[this] val queryChecker: List[QueryChecker] = List(
    new LocationChecker
  )
}

object DefinitionModule {
  import scala.io.Source

  def loadPredef(): Result[List[UserFunction]] = {
    val code = Source.fromFile("predef.smql", "UTF-8").getLines.mkString("\n")
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
  def loadUserdef(): Result[List[UserFunction]] = {
    val code = Source.fromFile("userdef.smql", "UTF-8").getLines.mkString("\n")
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
}
