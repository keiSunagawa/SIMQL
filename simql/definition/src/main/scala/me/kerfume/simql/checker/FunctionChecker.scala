package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._

trait FunctionChecker {
  def check(f: UserFunction, scope: Scope): Result[Scope]
}
