package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.list._

class TypeChecker extends FunctionChecker {
  def check(f: UserFunction, globalScope: Scope): Result[Scope] = {
    f.typeCheck(globalScope, Map(f.param.name -> f.param.tpe)).map(_ => globalScope)
  }
}
