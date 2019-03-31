package me.kerfume.simql.defun

import me.kerfume.simql._
import me.kerfume.simql.node._

object buildin {
  trait BuildInFunction extends SIMQLFunction {
    val body: List[Bind] = Nil
    val returnExpr: Expr = NullLit

    def getArg[T <: Expr](key: String, scope: Scope, ctx: QueryContext): T =
      scope(key)(Nil, scope, ctx).right.get.asInstanceOf[T]
  }
  object Add extends BuildInFunction {
    val key: String = "add"
    val params: List[FunctionParam] = List(NumberParam("num1"), NumberParam("num2"))
    val returnType: FunctionReturnType = NumberType

    override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
      val n1 = scope("num1")(Nil, scope, ctx).right.get.asInstanceOf[NumberLit].value
      val n2 = scope("num2")(Nil, scope, ctx).right.get.asInstanceOf[NumberLit].value
      Right(NumberLit(n1 + n2))
    }
  }
  object ConcatSymbol extends BuildInFunction {
    val key: String = "concat_symbol"
    val params: List[FunctionParam] = List(SymbolParam("a"), SymbolParam("b"))
    val returnType: FunctionReturnType = SymbolType

    override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
      val a = getArg[SymbolLit]("a", scope, ctx).label
      val b = getArg[SymbolLit]("b", scope, ctx).label
      Right(SymbolLit(a + b))
    }
  }
  object CtxGetTable extends BuildInFunction {
    val key: String = "ctx_get_table"
    val params: List[FunctionParam] = List(NumberParam("index"))
    val returnType: FunctionReturnType = SymbolType

    override def apply0(scope: Scope, ctx: QueryContext): Result[SymbolLit] = {
      val i = getArg[NumberLit]("index", scope, ctx).value
      ctx.tables.lift(i.toInt) match {
        case Some(tbl) => Right(tbl)
        case None      => Left(UnhandleError(s"table not found. index: $i"))
      }
    }
  }
  object SingleDot extends BuildInFunction {
    val key: String = "single_dot"
    val params: List[FunctionParam] = Nil
    val returnType: FunctionReturnType = SymbolType

    override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
      Right(SymbolLit("."))
    }
  }

  val functions: Scope = List(Add, ConcatSymbol, CtxGetTable, SingleDot).map(f => f.key -> f)(collection.breakOut)
}
