package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._

class FunctionCallChecker extends QueryChecker {
  def check(ast: Query, env: ASTMetaData): Result[Unit] = {
    FunctionCallCheckVisitor.visit(ast).run(env).map(_ => ())
  }
}

object FunctionCallCheckVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitFunctionCall(node: FunctionCall): RE[FunctionCall] = re { env =>
    SIMQLFunction.checkFunctionCall(node, env.globalScope).map(_ => node)
  }
}
