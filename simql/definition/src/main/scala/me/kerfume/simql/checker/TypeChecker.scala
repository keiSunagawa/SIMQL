package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.node.typeclass.TypeCheck

class TypeChecker extends FunctionChecker {
  def check(f: UserFunction, globalScope: Scope): Result[Scope] = {
    TypeCheck[UserFunction].check(f, globalScope, Map(f.param.name -> f.param.tpe)).map(_ => globalScope)
  }
}
