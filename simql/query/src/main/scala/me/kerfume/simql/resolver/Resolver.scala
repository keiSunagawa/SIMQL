package me.kerfume.simql.resolver

import me.kerfume.simql._
import me.kerfume.simql.node._

trait Resolver {
  protected[this] val visitor: ASTVisitor
  def resolve(ast: Query, ctx: QueryContext): Result[Query] = {
    visitor.visit(ast).run(ctx)
  }
}
