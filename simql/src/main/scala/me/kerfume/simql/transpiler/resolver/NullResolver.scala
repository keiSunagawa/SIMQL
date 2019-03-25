package me.kerfume.simql.transpiler.resolver

import me.kerfume.simql.transpiler._
import me.kerfume.simql.node.SimqlNode._
import cats.instances.either._
import scala.util.{Failure, Success, Try}

object NullResolver extends Resolver {
  def resolve(ast: SimqlRoot, meta: ASTMetaData): Either[String, SimqlRoot] = {
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
