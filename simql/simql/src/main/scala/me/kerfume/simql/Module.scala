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
      meta <- makeMetadata(ast)
      resolved <- resolvers.foldE(ast) { case (before, resolver) => resolver.resolve(before, meta) }
    } yield resolved
  }

  private[this] def makeMetadata(ast: Query): Result[ASTMetaData] = {
    val env = ASTMetaData.empty

    val analyzed = analyzer.analyze(ast, env)
    for {
      predef <- DefinitionModule.loadPredef()
      buildin = me.kerfume.simql.defun.buildin.functions
      gscope <- predef.foldE(buildin) {
                 case (s, f) =>
                   for {
                     _ <- (new RefChecker).check(f, s.map { case (key, _) => key -> () })
                     _ <- (new TypeChecker).check(f, s.map {
                           case (key, f) => key -> TypeChecker.FunctionSignature(f.params, f.returnType)
                         })
                   } yield s + (f.key -> f)
               }
    } yield
      analyzed.copy(
        globalScope = gscope
      )
  }

  private[this] val resolvers: List[Resolver] = List(
    new FunctionResolver
  )
}

object DefinitionModule {
  import scala.io.Source

  def loadPredef(): Result[List[SIMQLFunction]] = {
    val code = Source.fromFile("predef.smql", "UTF-8").getLines.mkString("\n")
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
}
