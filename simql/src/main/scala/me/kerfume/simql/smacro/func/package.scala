package me.kerfume.simql.smacro

import me.kerfume.simql.{ASTVisitor, Result}
import me.kerfume.simql.node.QueryNode._

package object func {
  type VarMap = Map[String, MacroArg]
  val right = Right(())

  class MacroArgsResolverVisitor(varMap: Map[String, MacroArg]) extends ASTVisitor {
    import ASTVisitor._
    import scala.reflect.ClassTag

    def resolve[Node <: MacroArg: ClassTag](node: MacroApply): Result[Node] = {
      varMap
        .get(node.symbol)
        .toRight(s"not found variable. key: ${node.symbol}; >< I want to print function name!!")
        .flatMap {
          case hs: Node => Right(hs)
          case _        => Left("macro function args parameter type error. this code is should be unreachable code :-/")
        }
    }

    override def visitHighSymbol(node: HighSymbol): RE[HighSymbol] = node match {
      case m: MacroApply =>
        re { _ =>
          resolve[HighSymbol](m)
        }
      case _ => super.visitHighSymbol(node)
    }

    override def visitTerm(node: Term): RE[Term] = node match {
      case m: MacroApply =>
        re { meta =>
          resolve[Term](m) match {
            case Right(n) => Right(n)
            case Left(_)  => super.visitTerm(node).run(meta)
          }
        }
      case _ => super.visitTerm(node)
    }

    override def visitCond(node: Cond): RE[Cond] = node match {
      case m: MacroApply =>
        re { meta =>
          resolve[Cond](m) match {
            case Right(n) => Right(n)
            case Left(_)  => super.visitCond(node).run(meta)
          }
        }
      case _ => super.visitCond(node)
    }
  }
}
