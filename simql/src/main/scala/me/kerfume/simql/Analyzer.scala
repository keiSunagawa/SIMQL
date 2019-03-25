package me.kerfume.simql

import me.kerfume.simql.node.QueryNode.Query

object Analyzer {
  def analyze(ast: Query): ASTMetaData = {
    val tables = ast.from.lhs +: ast.from.rhss.map(_.rhsTable)
    ASTMetaData(
      tables = tables,
      macroFuncs = Nil
    )
  }
}
