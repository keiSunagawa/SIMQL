package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.list._

class TypeChecker extends FunctionChecker {
  import SIMQLFunction._

  type ScopeValue = SIMQLFunction
  def check(f: SIMQLFunction, scope: CScope): Result[CScope] = {
    val withParam: CScope = scope ++ f.params.map(p => p.key -> variable(p.tpe))
    for {
      checked <- f.body.foldE(withParam) { case (acm, b) => checkBind(b, acm) }
      _ <- f.returnExpr match {
            case ff: FunctionCall =>
              for {
                ret <- checkFunctionCall(ff, checked)
                _ <- Either.cond(f.returnType.isAllowed(ret), (), ReturnTypeError(f.key, ret, f.returnType))
              } yield ()
            case other =>
              val ret = FunctionReturnType.fromExpr(other, Map.empty)
              Either.cond(f.returnType.isAllowed(ret), (), ReturnTypeError(f.key, ret, f.returnType))
          }
    } yield checked
  }

  private[this] def checkBind(b: Bind, scope: CScope): Result[CScope] = {
    def update(tpe: FunctionReturnType) = scope + (b.symbol -> variable(tpe))
    b.value match {
      case f: FunctionCall =>
        checkFunctionCall(f, scope).map(update)
      case _ =>
        val tpe = FunctionReturnType.fromExpr(b.value, Map.empty)
        Right(update(tpe))
    }
  }

  // use only type check
  private[this] def variable(retType: FunctionReturnType): SIMQLFunction = {
    new SIMQLFunction {
      val key: String = ""
      val params: List[FunctionParam] = Nil
      val returnType: FunctionReturnType = retType
      val body: List[Bind] = Nil
      val returnExpr: Expr = SymbolLit("")
    }
  }
}
