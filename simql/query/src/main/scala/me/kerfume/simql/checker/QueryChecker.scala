package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._

trait QueryChecker {
  def check(ast: Query, env: ASTMetaData): Result[Unit]
}
