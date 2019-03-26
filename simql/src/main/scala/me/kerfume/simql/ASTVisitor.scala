package me.kerfume.simql

import cats.instances.either._
import cats.instances.list._
import me.kerfume.simql.functions._
import me.kerfume.simql.node.QueryNode._

trait ASTVisitor {
  import ASTVisitor._

  def visitString(node: StringWrapper): RE[StringWrapper] = re { _ =>
    Right(node)
  }
  def visitNumber(node: NumberWrapper): RE[NumberWrapper] = re { _ =>
    Right(node)
  }
  def visitRaw(node: Raw): RE[Raw] = re { env =>
    for {
      args <- node.args.mapE(a => visitTerm(a).run(env))
    } yield node.copy(args = args)
  }
  def visitSymbol(node: SymbolWrapper): RE[SymbolWrapper] = re { _ =>
    Right(node)
  }
  def visitSymbolWithAccessor(node: SymbolWithAccessor): RE[SymbolWithAccessor] = re { _ =>
    Right(node)
  }
  def visitMacroApply(node: MacroApply): RE[MacroApply] = re { _ =>
    Right(node)
  }

  def visitHighSymbol(node: HighSymbol): RE[HighSymbol] = node match {
    case n: Raw                => visitRaw(n).map(identity)
    case n: SymbolWithAccessor => visitSymbolWithAccessor(n).map(identity)
    case n: MacroApply         => visitMacroApply(n).map(identity)
  }

  def visitTerm(node: Term): RE[Term] = node match {
    case RBracket(c)      => visitCond(c).map(RBracket)
    case n: StringWrapper => visitString(n).map(identity)
    case n: NumberWrapper => visitNumber(n).map(identity)
    case n: SymbolWrapper => visitSymbol(n).map(identity)
    case n: HighSymbol    => visitHighSymbol(n).map(identity)
    case NullLit =>
      re { _ =>
        Right(NullLit)
      }
  }

  def visitCond0(node: Cond0): RE[Cond0] = node match {
    case n: Term => visitTerm(n).map(identity)
    case BCond0(lhs, op, rhs) =>
      for {
        resLhs <- visitCond0(lhs)
        resRhs <- visitCond0(rhs)
      } yield BCond0(resLhs, op, resRhs)
  }
  def visitCond(node: Cond): RE[Cond] = node match {
    case n: Cond0 => visitCond0(n).map(identity)
    case BCond(lhs, op, rhs) =>
      for {
        resLhs <- visitCond(lhs)
        resRhs <- visitCond(rhs)
      } yield BCond(resLhs, op, resRhs)
  }

  def visitJoin(node: Join): RE[Join] = {
    for {
      rhsTable <- visitSymbol(node.rhsTable)
      on <- visitCond(node.on)
    } yield
      node.copy(
        rhsTable = rhsTable,
        on = on
      )
  }

  def visitFrom(node: From): RE[From] = {
    for {
      lhs <- visitSymbol(node.lhs)
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
                 node.values.mapE(n => visitHighSymbol(n).run(env))
               }
    } yield
      node.copy(
        values = values
      )
  }

  def visitWhere(node: Where): RE[Where] = {
    for {
      value <- visitCond(node.value)
    } yield
      node.copy(
        value = value
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
      head <- visitHighSymbol(node.head)
      tail <- re { env =>
               node.tail.mapE(n => visitHighSymbol(n).run(env))
             }
    } yield
      node.copy(
        head = head,
        tail = tail
      )
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
