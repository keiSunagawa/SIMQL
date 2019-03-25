package me.kerfume.simql.resolver

import me.kerfume.simql.node.QueryNode._
import cats.instances.either._
import me.kerfume.simql._

object NullResolver extends Resolver {
  def resolve(ast: Query, meta: ASTMetaData): Result[Query] = {
    NullResolverVisitor.visit(ast).run(meta)
  }
}

object NullResolverVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitCond(node: Cond): RE[Cond] = node match {
    case b: BinaryCond =>
      // 左辺がnullはスルー
      if (b.rhs == NullLit && b.op.op == BinaryOp.EQ) for {
        lhs <- visitHighSymbol(b.lhs)
      } yield IsNull(lhs)
      else if (b.rhs == NullLit && b.op.op == BinaryOp.NE) for {
        lhs <- visitHighSymbol(b.lhs)
      } yield IsNotNull(lhs)
      else super.visitCond(b)
    case _ => super.visitCond(node)
  }
}
