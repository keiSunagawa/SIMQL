package me.kerfume.simql

import cats.data.NonEmptyList
import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.list._
import cats.instances.either._

trait ASTVisitor {
  import ASTVisitor._

  def visitString(node: StringLit): RE[StringLit] = re { _ =>
    Right(node)
  }
  def visitNumber(node: NumberLit): RE[NumberLit] = re { _ =>
    Right(node)
  }
  def visitRaw(node: Raw): RE[Raw] = re { env =>
    for {
      args <- node.args.mapE(a => visitExpr(a).run(env))
    } yield node.copy(args = args)
  }
  def visitSymbol(node: SymbolLit): RE[SymbolLit] = re { _ =>
    Right(node)
  }
  def visitFunctionCall(node: FunctionCall): RE[FunctionCall] = re { _ =>
    Right(node)
  }

  def visitExpr(node: Expr): RE[Expr] = node match {
    case BExpr(lhs, op, rhs) =>
      for {
        resLhs <- visitExpr(lhs)
        resRhs <- visitExpr(rhs)
      } yield BExpr(resLhs, op, resRhs)
    case n: RBracket     => visitExpr(n.expr).map(RBracket)
    case n: FunctionCall => visitFunctionCall(n).map(identity)
    case n: Raw          => visitRaw(n).map(identity)
    case n: SymbolLit    => visitSymbol(n).map(identity)
    case n: StringLit    => visitString(n).map(identity)
    case n: NumberLit    => visitNumber(n).map(identity)
    case NullLit =>
      re { _ =>
        Right(NullLit)
      }
  }

  def visitJoin(node: Join): RE[Join] = {
    for {
      rhsTable <- visitExpr(node.rhsTable)
      on <- visitExpr(node.on)
    } yield
      node.copy(
        rhsTable = rhsTable,
        on = on
      )
  }

  def visitFrom(node: From): RE[From] = {
    for {
      lhs <- visitExpr(node.lhs)
      rhss <- re { env =>
               node.rhss.mapE(n => visitJoin(n).run(env))
             }
    } yield
      node.copy(
        lhs = node.lhs,
        rhss = rhss
      )
  }

  def visitSelect(node: Select): RE[Select] = {
    for {
      values <- re { env =>
                 node.columns.toList.mapE(n => visitExpr(n).run(env))
               }
    } yield {
      val h :: t = values
      node.copy(
        columns = NonEmptyList.of(h, t: _*)
      )
    }
  }

  def visitWhere(node: Where): RE[Where] = {
    for {
      expr <- visitExpr(node.expr)
    } yield
      node.copy(
        expr = expr
      )
  }

  def visitLimitOffset(node: LimitOffset): RE[LimitOffset] = {
    for {
      limit <- visitNumber(node.limit)
      offset <- re { env =>
                 transpose { node.offset.map(n => visitNumber(n).run(env)) }
               }
    } yield
      node.copy(
        limit = limit,
        offset = offset
      )
  }

  def visitOrder(node: Order): RE[Order] = {
    for {
      columns <- re { env =>
                  node.columns.toList.mapE(n => visitExpr(n).run(env))
                }
    } yield {
      val h :: t = columns
      node.copy(
        columns = NonEmptyList.of(h, t: _*)
      )
    }
  }

  def visit(node: Query): RE[Query] = {
    for {
      from <- visitFrom(node.from)
      select <- re { env =>
                 transpose { node.select.map(n => visitSelect(n).run(env)) }
               }
      where <- re { env =>
                transpose { node.where.map(n => visitWhere(n).run(env)) }
              }
      limitOffset <- re { env =>
                      transpose { node.limitOffset.map(n => visitLimitOffset(n).run(env)) }
                    }
      order <- re { env =>
                transpose { node.order.map(n => visitOrder(n).run(env)) }
              }
    } yield
      node.copy(
        from = from,
        select = select,
        where = where,
        limitOffset = limitOffset,
        order = order
      )
  }
}
object ASTVisitor {
  import cats.data.Kleisli
  // Reader Either
  type RE[A] = Kleisli[Result, ASTMetaData, A]
  def re[A](f: ASTMetaData => Result[A]): RE[A] =
    Kleisli[Result, ASTMetaData, A](f)
}
