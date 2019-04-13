package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.list._

object GenericsChecker {
  def check(f: SIMQLFunction): Result[Unit] = {
    val gs = f.defGenerics
    val hasFuncs = f.body.collect { case Bind(_, cf: SIMQLFunction) => cf }
    val dup = gs.toSet & hasFuncs.flatMap(_.defGenerics).toSet
    for {
      _ <- Either.cond(dup.isEmpty, (), UnhandleError(s"duplicate generics symbol. symbol: ${dup}"))
      _ <- hasFuncs.mapE(check)
    } yield ()
  }
}
