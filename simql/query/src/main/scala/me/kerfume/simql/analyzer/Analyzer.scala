package me.kerfume.simql.analyzer

import me.kerfume.simql._
import me.kerfume.simql.node._

trait Analyzer {
  def analyze(ast: Query, meta: ASTMetaData): ASTMetaData
}
