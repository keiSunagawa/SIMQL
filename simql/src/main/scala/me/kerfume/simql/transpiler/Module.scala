package me.kerfume.simql.transpiler

import me.kerfume.simql.node.SimqlNode.SimqlRoot
import parser._
import resolver._

object Module {
  def simqlToMysql(query: String): Either[TranspileError, MySQLGenerator.Code] = {
    for {
      ast <- parseAndResolve(query)
      mysql = MySQLGenerator.generate(ast)
    } yield mysql
  }

  private[this] def parseAndResolve(query: String): Either[TranspileError, SimqlRoot] = {
    for {
      ast <- Parser.parseSimql(query).toRight("failed parse.")
      meta = makeMetadata(ast)
      resolved <- resolvers.foldLeft[Either[TranspileError, SimqlRoot]](Right(ast)) {
                   case (before, resolver) =>
                     before match {
                       case Right(target) => resolver.resolve(target, meta)
                       case Left(e)       => Left(e)
                     }
                 }
    } yield resolved
  }

  private[this] def makeMetadata(ast: SimqlRoot): ASTMetaData = {
    import me.kerfume.simql.transpiler.querymacro.buildin._
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
