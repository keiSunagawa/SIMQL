package me.kerfume.simql.analyzer

import me.kerfume.simql._
import me.kerfume.simql.node._

class TableAnalyzer extends Analyzer {
  override def analyze(ast: Query, meta: ASTMetaData): ASTMetaData = {
    val tables = ast.from.lhs +: ast.from.rhss.map(_.rhsTable)
    meta.copy(
      tables = meta.tables ++ tables
    )
  }
}
