package me.kerfume.simql

import me.kerfume.simql.node.QueryNode.Query
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.resolver.{AccessorResolver, MacroFuncResolver, NullResolver, Resolver}
import me.kerfume.simql.generator.MySQLGenerator

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
      meta = makeMetadata(ast)
      resolved <- resolvers.foldLeft[Result[Query]](Right(ast)) {
                   case (before, resolver) =>
                     before match {
                       case Right(target) => resolver.resolve(target, meta)
                       case Left(e)       => Left(e)
                     }
                 }
    } yield resolved
  }

  private[this] def makeMetadata(ast: Query): ASTMetaData = {
    import me.kerfume.simql.smacro.func.buildin._
    val analyzed = Analyzer.analyze(ast)
    analyzed.copy(
      macroFuncs = List(
        Count
      )
    )
  }

  private[this] val resolvers: Seq[Resolver] = Seq(
    MacroFuncResolver,
    AccessorResolver,
    NullResolver
  )
}
