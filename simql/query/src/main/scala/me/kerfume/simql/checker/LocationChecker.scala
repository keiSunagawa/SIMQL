package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.either._
import cats.instances.list._
import cats.data.NonEmptyList

class LocationChecker extends QueryChecker {
  def check(ast: Query, ctx: QueryContext): Result[Unit] = {
    LocationCheckVisitor.visit(ast).run(ctx).map(_ => ())
  }
}

object LocationCheckVisitor extends ASTVisitor {
  import ASTVisitor._

  override def visitJoin(node: Join): RE[Join] = re { ctx =>
    for {
      _ <- Either.cond(allowedToTable(node.rhsTable, ctx.globalScope), (), UnhandleError("illegal location."))
      res <- super.visitJoin(node).run(ctx)
    } yield res
  }

  override def visitFrom(node: From): RE[From] = re { ctx =>
    for {
      _ <- Either.cond(allowedToTable(node.lhs, ctx.globalScope), (), UnhandleError("illegal location."))
      res <- super.visitFrom(node).run(ctx)
    } yield res
  }

  override def visitSelect(node: Select): RE[Select] = re { ctx =>
    for {
      _ <- node.columns.toList.mapE { c =>
            Either.cond(allowedToColumn(c, ctx.globalScope), (), UnhandleError("illegal location."))
          }
      res <- super.visitSelect(node).run(ctx)
    } yield res
  }

  override def visitOrder(node: Order): RE[Order] = re { ctx =>
    for {
      _ <- node.columns.toList.mapE { c =>
            Either.cond(allowedToColumn(c, ctx.globalScope), (), UnhandleError("illegal location."))
          }
      res <- super.visitOrder(node).run(ctx)
    } yield res
  }

  private[this] def allowedToColumn(expr: Expr, scope: Scope): Boolean = isAllowedType(expr, scope) {
    case _: SymbolLit =>
    case _: Raw       =>
    // TODO should allowed expr?
  }
  private[this] def allowedToTable(expr: Expr, scope: Scope): Boolean = isAllowedType(expr, scope) {
    case _: SymbolLit =>
    case _: Raw       =>
  }
  private[this] def isAllowedType(expr: Expr, scope: Scope)(allowedTo: PartialFunction[Expr, Unit]): Boolean = {
    def convert(tpe: FunctionReturnType): Expr = tpe match {
      case StringType => StringLit("")
      case NumberType => NumberLit(0)
      case SymbolType => SymbolLit("")
      case ExprType   => BExpr(NullLit, Op(ExprOp.EQ), NullLit)
    }
    expr match {
      case f: FunctionCall =>
        val retType = scope(f.symbol) |> (sf => convert(sf.returnType))
        allowedTo.isDefinedAt(retType)
      case other =>
        allowedTo.isDefinedAt(other)
    }
  }
}
