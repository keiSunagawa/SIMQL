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

  def simqlToMysql(
    query: String,
    predef: Option[String] = None,
    userdef: Option[String] = None
  ): Result[MySQLGenerator.Code] = {
    for {
      ast <- parseAndResolve(query.trim, predef.map(_.trim), userdef.map(_.trim))
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
      result <- DefinitionModule.compile((predef.toSeq ++ userdef.toSeq): _*)
    } yield
      analyzed.copy(
        globalScope = result._1,
        typeMap = result._2
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
  def compile(code: String*): Result[(Scope, Map[String, SIMQLType])] = {
    val buildin = me.kerfume.simql.defun.buildin.functions
    for {
      buildinTypeMap <- buildin.toList.mapE { case (key, f) => TypeCheck[Value].check(f, Map.empty).map(key -> _) }
                         .map(_.toMap) // FIXME
      parsed <- code.toList.mapE { c =>
                 Parser.parseDefinition(c).toRight(UnhandleError("failed parse."))
               }.map(_.flatten)
      typeMap <- parsed.foldE(buildinTypeMap) {
                  case (pm, f) =>
                    for {
                      _ <- GenericsChecker.check(f)
                      tpe <- TypeCheck[SIMQLFunction].check(f, pm)
                    } yield pm + (f.key -> tpe)
                }
    } yield (buildin ++ parsed.map(f => f.key -> Pure(f))) -> typeMap
  }
  def loadPredef(code: String): Result[List[UserFunction]] = {
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
  def loadUserdef(code: String): Result[List[UserFunction]] = {
    Parser.parseDefinition(code).toRight(UnhandleError("failed parse."))
  }
}
