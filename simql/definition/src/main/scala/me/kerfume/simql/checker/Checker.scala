package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._

trait Checker {
  type ScopeValue
  type CScope = Map[String, ScopeValue]
  def check(f: SIMQLFunction, scope: CScope): Result[CScope]
}
