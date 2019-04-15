package me.kerfume.simql

import me.kerfume.simql.node._
import me.kerfume.simql.node.typeclass.TypeCheck
import me.kerfume.simql.analyzer._
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.resolver._
import me.kerfume.simql.generator.MySQLGenerator
import me.kerfume.simql.functions._
import cats.instances.list._
import me.kerfume.simql.checker._

object Module {
  private[this] val analyzer = new TableAnalyzer

  def simqlToMysql(query: String, predef: Option[String] = None, userdef: Option[String] = None): Result[MySQLGenerator.Code] = {
    for {
      ast <- parseAndResolve(query, predef, userdef)
      mysql = MySQLGenerator.generate(ast)
    } yield mysql
  }

  private[this] def parseAndResolve(query: String, predef: Option[String], userdef: Option[String]): Result[Query] = {
    for {
      ast <- Parser.parseSimql(query).toRight(UnhandleError("failed parse."))
      meta <- makeContext(ast, predef, userdef)
      resolved <- resolvers.foldE(ast) { case (before, resolver) => resolver.resolve(before, meta) }
      _ <- queryChecker.mapE(_.check(resolved, meta))
    } yield resolved
  }

  private[this] def makeContext(ast: Query, predef: Option[String], userdef: Option[String]): Result[QueryContext] = {
    val ctx = QueryContext.empty

    val analyzed = analyzer.analyze(ast, ctx)
    for {
      predef <- predef.map(DefinitionModule.loadPredef).getOrElse(Right(Nil))
      userdef <- userdef.map(DefinitionModule.loadUserdef).getOrElse(Right(Nil))
      buildin = me.kerfume.simql.defun.buildin.functions
      gscope = buildin ++ (predef ++ userdef).map(f => f.key -> Pure(f))
      buildinTypeMap <- buildin.toList.mapE { case (key, f) => TypeCheck[Value].check(f, Map.empty).map(key -> _) }
                         .map(_.toMap) // FIXME
      typeMap <- (predef ++ userdef).foldE(buildinTypeMap) {
                  case (pm, f) =>
                    for {
                      _ <- GenericsChecker.check(f)
                      tpe <- TypeCheck[SIMQLFunction].check(f, pm)
                    } yield pm + (f.key -> tpe)
                }
    } yield
      analyzed.copy(
        globalScope = gscope,
        typeMap = typeMap
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
  def loadPredef(code: String): Result[List[UserFunction]] = {
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
  def loadUserdef(code: String): Result[List[UserFunction]] = {
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
}
