package me.kerfume.simql.transpiler.resolver

import me.kerfume.simql.transpiler._
import me.kerfume.simql.node.SimqlNode._
import cats.instances.either._

object MacroFuncResolver extends Resolver {
  def resolve(ast: SimqlRoot, meta: ASTMetaData): Either[String, SimqlRoot] = {
    MacroFuncResolverVisitor.visit(ast).run(meta)
  }
}

object MacroFuncResolverVisitor extends ASTVisitor {
  import ASTVisitor._
  import me.kerfume.simql.transpiler.querymacro.MacroFunc._

  override def visitTerm(node: Term): RE[Term] =
    node match {
      case n: HighSymbol =>
        n match {
          case n: MacroApply =>
            // TODO implements Term macro
            resolve0().map(identity)
          case _ => super.visitHighSymbol(n).map(identity)
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
                       case None    => Left(s"not define macro. symbol: $key")
                     }
          visited <- super.visitCond(resolved).run(meta)
        } yield visited
      }
    case _ => super.visitCond(node)
  }

  def resolve0(): RE[HighSymbol] = re { _ =>
    Right(Raw("UN IMPLEMENTS", Nil))
  }
}
