package me.kerfume.simql.resolver

import me.kerfume.simql._
import me.kerfume.simql.node._
import cats.instances.either._

class NullResolver extends Resolver {
  val visitor = NullResolveVisitor
}

object NullResolveVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitExpr(node: Expr): RE[Expr] = node match {
    case BExpr(lhs, Op(op), rhs) if (rhs == NullLit && (op == ExprOp.EQ || op == ExprOp.NE)) =>
      for {
        resLhs <- visitExpr(lhs)
        resRhs <- visitExpr(rhs)
      } yield if (op == ExprOp.EQ) Call("is_null", List(resLhs)) else Call("is_not_null", List(resLhs))
    case other => super.visitExpr(other)
  }
}
