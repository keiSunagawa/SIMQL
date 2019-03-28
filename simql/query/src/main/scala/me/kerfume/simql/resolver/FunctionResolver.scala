package me.kerfume.simql.resolver

import me.kerfume.simql._
import me.kerfume.simql.node._

class FunctionResolver extends Resolver {
  val visitor = FunctionResolveVisitor
}

object FunctionResolveVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitExpr(node: Expr): RE[Expr] = node match {
    case f: FunctionCall =>
      re { env =>
        env.globalScope.get(f.symbol) match {
          case Some(sf) =>
            sf.apply(f.args, env.globalScope, env)
          case None => Left(FunctionNotFound(f.symbol))
        }

      }
    case other => super.visitExpr(other)
  }
}
