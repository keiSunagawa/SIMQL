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
  object calc {
    val values = List(Add, Sub, Mul, Div, ConcatString, ConcatSymbol)

    trait BinaryArithmetic extends BuildInFunction {
      protected[this] val calc: (BigDecimal, BigDecimal) => BigDecimal

      val params: List[FunctionParam] = List(NumberParam("a"), NumberParam("b"))
      val returnType: FunctionReturnType = NumberType

      override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
        val a = getArg[NumberLit]("a", scope, ctx).value
        val b = getArg[NumberLit]("b", scope, ctx).value
        Right(NumberLit(calc(a, b)))
      }
    }
    object Add extends BinaryArithmetic {
      val key: String = "add"
      val calc = (_ + _)
    }
    object Sub extends BinaryArithmetic {
      val key: String = "sub"
      val calc = (_ - _)
    }
    object Mul extends BinaryArithmetic {
      val key: String = "mul"
      val calc = (_ * _)
    }
    object Div extends BinaryArithmetic {
      val key: String = "div"
      val calc = { (a, b) =>
        if (b != 0) a / b else 0
      }
    }
    object ConcatString extends BuildInFunction {
      val key: String = "concat_string"
      val params: List[FunctionParam] = List(StringParam("a"), StringParam("b"))
      val returnType: FunctionReturnType = StringType

      override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
        val a = getArg[StringLit]("a", scope, ctx).value
        val b = getArg[StringLit]("b", scope, ctx).value
        Right(StringLit(a + b))
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
  }
  object context {
    val values = List(CtxGetTable)

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
  }
  object constants {
    val values = List(SingleDot)

    object SingleDot extends BuildInFunction {
      val key: String = "single_dot"
      val params: List[FunctionParam] = Nil
      val returnType: FunctionReturnType = SymbolType

      override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
        Right(SymbolLit("."))
      }
    }
  }
  object develop {
    val values = List(Eval, Debug)

    object Eval extends BuildInFunction {
      val key: String = "eval"
      val params: List[FunctionParam] = List(ExprParam("value"))
      val returnType: FunctionReturnType = ExprType

      override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
        val value = getArg[Expr]("value", scope, ctx)
        val evaled = value.eval(scope, ctx)
        Right(value)
      }
    }
    object Debug extends BuildInFunction {
      val key: String = "debug"
      val params: List[FunctionParam] = List(ExprParam("value"))
      val returnType: FunctionReturnType = ExprType

      override def apply0(scope: Scope, ctx: QueryContext): Result[Expr] = {
        val value = getArg[Expr]("value", scope, ctx)
        println(value)
        Right(value)
      }
    }
  }

  val functions: Scope =
    (calc.values ++ context.values ++ constants.values ++ develop.values).map(f => f.key -> f)(collection.breakOut)
}
