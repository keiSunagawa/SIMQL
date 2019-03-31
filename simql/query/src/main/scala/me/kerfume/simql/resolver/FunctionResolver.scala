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
      re { ctx =>
        ctx.globalScope.get(f.symbol) match {
          case Some(sf) =>
            sf.apply(f.args, ctx.globalScope, ctx)
          case None => Left(FunctionNotFound(f.symbol))
        }

      }
    case other => super.visitExpr(other)
  }
}
