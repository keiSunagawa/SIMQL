package me.kerfume.simql.transpiler

import me.kerfume.simql.node.SimqlNode._
import me.kerfume.simql.functions._
import cats.instances.list._
import cats.instances.either._

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
    case n: StringWrapper => visitString(n).map(identity)
    case n: NumberWrapper => visitNumber(n).map(identity)
    case n: SymbolWrapper => visitSymbol(n).map(identity)
    case n: HighSymbol    => visitHighSymbol(n).map(identity)
    case NullLit =>
      re { _ =>
        Right(NullLit)
      }
  }

  def visitBinaryCond(node: BinaryCond): RE[BinaryCond] = {
    for {
      lhs <- visitHighSymbol(node.lhs)
      rhs <- visitTerm(node.rhs)
    } yield
      node.copy(
        lhs = lhs,
        rhs = rhs
      )
  }
  def visitIsNull(node: IsNull): RE[IsNull] = {
    for {
      lhs <- visitHighSymbol(node.lhs)
    } yield
      node.copy(
        lhs = lhs
      )
  }
  def visitIsNotNull(node: IsNotNull): RE[IsNotNull] = {
    for {
      lhs <- visitHighSymbol(node.lhs)
    } yield
      node.copy(
        lhs = lhs
      )
  }

  def visitCond(node: Cond): RE[Cond] = node match {
    case n: BinaryCond => visitBinaryCond(n).map(identity)
    case n: IsNull     => visitIsNull(n).map(identity)
    case n: IsNotNull  => visitIsNotNull(n).map(identity)
    case n: Raw        => visitRaw(n).map(identity)
    case n: MacroApply => ??? // TODO
  }

  def visitExprRhs(node: ExprRhs): RE[ExprRhs] = {
    for {
      value <- visitCond(node.value)
    } yield
      node.copy(
        value = value
      )
  }

  def visitExpr(node: Expr): RE[Expr] = {
    for {
      lhs <- visitCond(node.lhs)
      rhss <- re { env =>
               node.rhss.mapE(n => visitExprRhs(n).run(env))
             }
    } yield
      node.copy(
        lhs = lhs,
        rhss = rhss
      )
  }

  def visitJoin(node: Join): RE[Join] = {
    for {
      rhsTable <- visitSymbol(node.rhsTable)
      on <- visitExpr(node.on)
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
      value <- visitExpr(node.value)
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

  def visit(node: SimqlRoot): RE[SimqlRoot] = {
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
  type RE[A] = Kleisli[({ type F[B] = Either[TranspileError, B] })#F, ASTMetaData, A]
  def re[A](f: ASTMetaData => Either[TranspileError, A]): RE[A] =
    Kleisli[({ type F[B] = Either[TranspileError, B] })#F, ASTMetaData, A](f)
}
