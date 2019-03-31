package me.kerfume.simql.analyzer

import me.kerfume.simql._
import me.kerfume.simql.node._

class TableAnalyzer extends Analyzer {
  override def analyze(ast: Query, ctx: QueryContext): QueryContext = {
    val tables = ast.from.lhs.asInstanceOf[SymbolLit] +: ast.from.rhss.map(_.rhsTable.asInstanceOf[SymbolLit])
    ctx.copy(
      tables = ctx.tables ++ tables
    )
  }
}
