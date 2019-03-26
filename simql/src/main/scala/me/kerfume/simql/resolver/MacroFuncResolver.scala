package me.kerfume.simql.resolver

import me.kerfume.simql.node.QueryNode._
import me.kerfume.simql._

object MacroFuncResolver extends Resolver {
  def resolve(ast: Query, meta: ASTMetaData): Result[Query] = {
    MacroFuncResolverVisitor.visit(ast).run(meta)
  }
}

object MacroFuncResolverVisitor extends ASTVisitor {
  import ASTVisitor._
  import me.kerfume.simql.smacro.MacroFunc._

  override def visitTerm(node: Term): RE[Term] =
    node match {
      case MacroApply(key, args) =>
        re { meta =>
          meta.macroFuncs.termMacros.get(key) match {
            case Some(f) => f.apply(args)
            case None    => super.visitTerm(node).run(meta) // 末端で確実にLeftを返せるならよいが少々危険か…?
          }
        }
      case _ => super.visitTerm(node)
    }

  override def visitHighSymbol(node: HighSymbol): RE[HighSymbol] = node match {
    case MacroApply(key, args) =>
      re { meta =>
        meta.macroFuncs.highSymbolMacros.get(key) match {
          case Some(f) => f.apply(args)
          case None    => Left(s"not define macro. symbol: $key")
        }
      }
    case _ => super.visitHighSymbol(node)
  }

  override def visitCond(node: Cond): RE[Cond] = node match {
    case MacroApply(key, args) =>
      re { meta =>
        for {
          resolved <- meta.macroFuncs.condMacros.get(key) match {
                       case Some(f) => f.apply(args)
                       case None    => super.visitCond(node).run(meta)
                     }
          visited <- super.visitCond(resolved).run(meta)
        } yield visited
      }
    case _ => super.visitCond(node)
  }
}
