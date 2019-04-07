package me.kerfume.simql.node.typeclass

import cats.instances.list._
import me.kerfume.simql._
import me.kerfume.simql.functions._
import me.kerfume.simql.node.{ typeclass => _, _ }
import simulacrum._

@typeclass trait Eval[A] {
  def eval(a: A, scope: Scope, ctx: QueryContext): Result[Expr]
}

trait LowPriority {
  import Eval.instance
  implicit def atomicEval[T <: Atomic]: Eval[T] = instance { (a, _, _) => Right(a) }
}
object Eval extends LowPriority {
  def instance[A](f: (A, Scope, QueryContext) => Result[Expr]): Eval[A] = new Eval[A] {
    override def eval(a: A, scope: Scope, ctx: QueryContext): Result[Expr] = f(a, scope, ctx)
  }
  implicit val functionEval: Eval[SIMQLFunction] = instance { (a, scope, _) =>
    Right(a.setOuterScope(scope))
  }
  implicit val bexprEval: Eval[BExpr] = instance { (a, scope, ctx) =>
    for {
      rhs <- Eval[Expr].eval(a.rhs, scope, ctx)
      lhs <- Eval[Expr].eval(a.lhs, scope, ctx)
    } yield a.copy(
      rhs = rhs,
      lhs = lhs,
    )
  }
  implicit val rawEval: Eval[Raw] = instance  { (a, scope, ctx) =>
    for {
      args <- a.args.mapE {Eval[Expr].eval(_, scope, ctx)}
    } yield a.copy(args = args)
  }
  implicit val rbracketEval: Eval[RBracket] = instance  { (a, scope, ctx) =>
    for {
      expr <- Eval[Expr].eval(a.expr, scope, ctx)
    } yield a.copy(expr = expr)
  }
  implicit val callEval: Eval[Call] = instance { (a, scope, ctx) =>
    for {
      value <- scope.get(a.symbol).toRight(FunctionNotFound(a.symbol))
      unwrapped <- Eval[Value].eval(value, scope, ctx)
      res <- unwrapped match {
        case ff: SIMQLFunction => ff.apply(a.args.map(Thunk(_, scope)), ctx)
        case other             => Right(other)
      }
    } yield res
  }
  implicit val exprEval: Eval[Expr] = instance { (a, scope, ctx) =>  // うーん...
    a match {
      case n: SIMQLFunction => Eval[SIMQLFunction].eval(n, scope, ctx)
      case n: Atomic => Eval[Atomic].eval(n, scope, ctx)
      case n: BExpr => Eval[BExpr].eval(n, scope, ctx)
      case n: Raw => Eval[Raw].eval(n, scope, ctx)
      case n: RBracket => Eval[RBracket].eval(n, scope, ctx)
      case n :Call => Eval[Call].eval(n, scope, ctx)
    }
  }
  implicit val thunkEval: Eval[Thunk] = instance { (a, _, ctx) =>
    Eval[Expr].eval(a.value, a.scope, ctx)
  }
  implicit val pureEval: Eval[Pure] = instance { (a, scope, ctx) =>
    Eval[Expr].eval(a.value, scope, ctx)
  }
  implicit val valueEval: Eval[Value] = instance { (a, scope, ctx) =>
    a match {
      case v: Thunk => Eval[Thunk].eval(v, scope, ctx)
      case v: Pure  => Eval[Pure].eval(v, scope, ctx)
    }
  }
}
