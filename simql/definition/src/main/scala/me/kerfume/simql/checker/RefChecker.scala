package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.list._

class RefChecker extends FunctionChecker {
  import RefChecker._

  type ScopeValue = Unit
  def check(f: SIMQLFunction, scope: CScope): Result[CScope] = {
    val withParam: CScope = scope ++ f.params.map(_.key -> ())(collection.breakOut)
    for {
      checked <- f.body.foldE(withParam) { case (acm, b) => checkBind(b, acm) }
      _ <- f.returnExpr match {
            case ff: FunctionCall => checkFunctionCall(ff, checked)
            case _                => Right(())
          }
    } yield checked
  }

  private[this] def checkBind(b: Bind, scope: CScope): Result[CScope] = {
    def update = scope + (b.symbol -> ())
    b.value match {
      case f: FunctionCall =>
        checkFunctionCall(f, scope).map(_ => update)
      case _ => Right(update)
    }
  }
  private[this] def checkFunctionCall(f: FunctionCall, scope: CScope): Result[Unit] = {
    for {
      _ <- assertRef(f.symbol, scope)
      _ <- f.args.mapE {
            case ff: FunctionCall => checkFunctionCall(ff, scope)
            case _                => Right(())
          }
    } yield ()
  }

  private[this] def assertRef(symbol: String, scope: CScope /* ref error args */ ): Result[Unit] =
    Either.cond(scope.contains(symbol), (), RefError(symbol))
}

object RefChecker {
  case class RefError(symbol: String) extends SIMQLError
}
