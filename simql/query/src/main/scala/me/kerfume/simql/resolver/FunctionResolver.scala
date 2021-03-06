package me.kerfume.simql.resolver

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.node.typeclass.{Eval, TypeCheck}

class FunctionResolver extends Resolver {
  val visitor = FunctionResolveVisitor
}

object FunctionResolveVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitExpr(node: Expr): RE[Expr] = node match {
    case f: Call =>
      re { ctx =>
        for {
          _ <- TypeCheck[Call].check(f, ctx.typeMap) // call args type check.
          value <- Eval[Call].eval(f, ctx.globalScope, ctx)
          ret <- value match {
                  case ff: SIMQLFunction =>
                    ff(f.args.map(Pure), ctx)
                  case other => Right(other)
                }
        } yield ret
      }
    case other => super.visitExpr(other)
  }
}
