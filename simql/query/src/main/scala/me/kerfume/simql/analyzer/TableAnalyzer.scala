package me.kerfume.simql.analyzer

import me.kerfume.simql._
import me.kerfume.simql.node._

class TableAnalyzer extends Analyzer {
  override def analyze(ast: Query, ctx: QueryContext): QueryContext = {
    // TODO recover Call class
    val tables = (if (ast.from.lhs.isInstanceOf[SymbolLit]) List(ast.from.lhs.asInstanceOf[SymbolLit]) else Nil) ++
      ast.from.rhss.collect{ case a if a.rhsTable.isInstanceOf[SymbolLit] => a.rhsTable.asInstanceOf[SymbolLit] }
    ctx.copy(
      tables = ctx.tables ++ tables
    )
  }
}
