package me.kerfume.simql.defun

import me.kerfume.simql._
import me.kerfume.simql.node.SIMQLFunction._
import me.kerfume.simql.node._

object buildin {
  val functions: Scope = (calc.values ++ context.values ++ control.values ++ develop.values)
    .map(f => f.key -> Pure(f))
    .toMap ++ constants.values

  trait BuildInFunction extends SIMQLFunction {
    val body: List[Bind] = Nil
    val returnValue: Expr = NullLit
    val outerScope: Scope = Map.empty

    protected[this] def getArg[T <: Expr](key: String, scope: Scope, ctx: QueryContext): T = {
      def valueMatch(value: Value): T = {
        value match {
          case v: Thunk =>
            v.eval(ctx).right.get.asInstanceOf[T]
          case v: Pure =>
            v.eval(scope, ctx).right.get.asInstanceOf[T]
        }
      }
      valueMatch(scope(key))
    }
  }

  private[this] def makeBuildInFunction(
    name: String,
    args: List[FunctionParam],
    implReturnType: SIMQLType
  )(
    _apply: (Scope, QueryContext) => Result[Expr]
  ): SIMQLFunction = {
    val impl = new BuildInFunction {
      override val key: String = name
      override val param: FunctionParam = args.last
      override val returnType: SIMQLType = implReturnType

      override def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr] = _apply(scope, ctx)

      override def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] =
        Right(FunctionType(this.param.tpe, this.returnType))
    }
    args.init.foldRight[SIMQLFunction](impl) {
      case (p, next) =>
        val nextType = FunctionType(next.param.tpe, next.returnType)
        Closure(key = name, param = p, returnType = nextType, body = Nil, returnValue = next)
    }
  }

  private[this] def getArg[T <: Expr](key: String, scope: Scope, ctx: QueryContext): T = {
    def valueMatch(value: Value): T = {
      value match {
        case v: Thunk =>
          v.eval(ctx).right.get.asInstanceOf[T] // TODO left時のエラーハンドリング
        case v: Pure =>
          v.eval(scope, ctx).right.get.asInstanceOf[T] // TODO left時のエラーハンドリング
      }
    }
    valueMatch(scope(key))
  }

  object calc {
    private[this] def binaryArithmetic(name: String)(_apply: (BigDecimal, BigDecimal) => BigDecimal): SIMQLFunction =
      makeBuildInFunction(name, List(FunctionParam("a", NumberType), FunctionParam("b", NumberType)), NumberType) {
        (scope, ctx) =>
          val a = getArg[NumberLit]("a", scope, ctx).value
          val b = getArg[NumberLit]("b", scope, ctx).value
          Right(NumberLit(_apply(a, b)))
      }
    val Add = binaryArithmetic("add")(_ + _)
    val Sub = binaryArithmetic("sub")(_ - _)
    val Mul = binaryArithmetic("mul")(_ * _)
    val Div = binaryArithmetic("div") { (a, b) =>
      if (b != 0) a / b else 0
    }

    val ConcatString =
      makeBuildInFunction("cst", List(FunctionParam("a", StringType), FunctionParam("b", StringType)), StringType) {
        (scope, ctx) =>
          val a = getArg[StringLit]("a", scope, ctx).value
          val b = getArg[StringLit]("b", scope, ctx).value
          Right(StringLit(a + b))
      }

    val ConcatSymbol =
      makeBuildInFunction("csm", List(FunctionParam("a", SymbolType), FunctionParam("b", SymbolType)), SymbolType) {
        (scope, ctx) =>
          val a = getArg[SymbolLit]("a", scope, ctx).label
          val b = getArg[SymbolLit]("b", scope, ctx).label
          Right(SymbolLit(a + b))
      }

    val Eq = makeBuildInFunction("eq", List(FunctionParam("a", Generics), FunctionParam("b", Generics)), BooleanType) {
      (scope, ctx) =>
        val a = getArg[Expr]("a", scope, ctx)
        val b = getArg[Expr]("b", scope, ctx)
        Right(BooleanLit(a == b))
    }
    val Not = makeBuildInFunction("not", List(FunctionParam("bool", BooleanType)), BooleanType) { (scope, ctx) =>
      val bool = getArg[BooleanLit]("bool", scope, ctx).value
      Right(BooleanLit(!bool))
    }

    val values = List(Add, Sub, Mul, Div, ConcatString, ConcatSymbol, Eq, Not)
  }

  object control {
    val If = makeBuildInFunction(
      "if",
      List(FunctionParam("cond", BooleanType), FunctionParam("then", Generics), FunctionParam("else", Generics)),
      Generics
    ) { (scope, ctx) =>
      val cond = getArg[BooleanLit]("cond", scope, ctx).value
      if (cond) Right(getArg[Expr]("then", scope, ctx))
      else Right(getArg[Expr]("else", scope, ctx))
    }

    val values = List(If)
  }
  object context {
    val GetTable = makeBuildInFunction("get_table", List(FunctionParam("index", NumberType)), SymbolType) {
      (scope, ctx) =>
        val i = getArg[NumberLit]("index", scope, ctx).value
        ctx.tables.lift(i.toInt) match {
          case Some(tbl) => Right(tbl)
          case None      => Left(UnhandleError(s"table not found. index: $i"))
        }
    }

    val values = List(GetTable)
  }
  object constants {
    val SingleDot = "single_dot" -> Pure(SymbolLit("."))
    val values: Scope = Map(SingleDot)
  }

  object develop {
    val Debug = makeBuildInFunction("dbg", List(FunctionParam("value", Generics)), Generics) { (scope, ctx) =>
      val value = getArg[Expr]("value", scope, ctx)
      println(value)
      Right(value)
    }

    val values = List(Debug)
  }
}
