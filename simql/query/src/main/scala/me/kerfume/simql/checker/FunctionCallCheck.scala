package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._

class FunctionCallChecker extends QueryChecker {
  def check(ast: Query, ctx: QueryContext): Result[Unit] = {
    FunctionCallCheckVisitor.visit(ast).run(ctx).map(_ => ())
  }
}

object FunctionCallCheckVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitFunctionCall(node: FunctionCall): RE[FunctionCall] = re { ctx =>
    for {
      _ <- Either.cond(ctx.globalScope.contains(node.symbol), (), FunctionNotFound(node.symbol))
      _ <- SIMQLFunction.checkFunctionCall(node, ctx.globalScope)
    } yield node
  }
}
