package me.kerfume.simql.defun

import me.kerfume.simql.defun.buildin.Converter.ToList
import me.kerfume.simql._
import me.kerfume.simql.node.SIMQLFunction._
import me.kerfume.simql.node._
import me.kerfume.simql.node.typeclass.Eval
import shapeless._

object buildin {
  val functions: Scope = (calc.values ++ context.values ++ list.values ++ control.values ++ develop.values)
    .map(f => f.key -> Pure(f))
    .toMap ++ constants.values

  private[this] def getArg[T <: Expr](key: String, scope: Scope, ctx: QueryContext): Result[T] =
    Eval[Value].eval(scope(key), scope, ctx).map(_.asInstanceOf[T])

  trait BuildInFunction extends SIMQLFunction {
    val body: List[Bind] = Nil
    val returnValue: Expr = NullLit
    val outerScope: Scope = Map.empty
  }

  private[this] def unevalArgsFunction(
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

  private[this] case class FParam[T <: SIMQLType](key: String, tpe: T) {
    def toFunctionParam: FunctionParam = FunctionParam(key, tpe)
  }
  private[this] type P[T <: SIMQLType] = FParam[T]
  implicit final private[this] class TypeBinder(key: String) {
    def \>[T <: SIMQLType](tpe: T): FParam[T] = FParam(key, tpe)
  }

  // TODO mapでいけそう
  sealed trait Converter[I] {
    type O
    def convert(in: I, scope: Scope, ctx: QueryContext): Result[O]
  }
  object Converter {
    type AUX[I0, O0] = Converter[I0] { type O = O0 }
    type \:>[I0 <: SIMQLType, O0] = AUX[P[I0], O0]
    def simqlTypeC[F <: SIMQLType, T <: Expr]: Converter.AUX[P[F], T] = new Converter[P[F]] {
      type O = T
      def convert(in: P[F], scope: Scope, ctx: QueryContext): Result[O] = {
        getArg[O](in.key, scope, ctx)
      }
    }
    implicit val strC: StringType.type \:> StringLit = simqlTypeC[StringType.type, StringLit]
    implicit val boolC: BooleanType.type \:> BooleanLit = simqlTypeC[BooleanType.type, BooleanLit]
    implicit val symC: SymbolType.type \:> SymbolLit = simqlTypeC[SymbolType.type, SymbolLit]
    implicit val numC: NumberType.type \:> NumberLit = simqlTypeC[NumberType.type, NumberLit]
    implicit val rawC: RawType.type \:> Raw = simqlTypeC[RawType.type, Raw]
    implicit val exprC: ExprType.type \:> Expr = simqlTypeC[ExprType.type, Expr]
    implicit val listC: ListType \:> SIMQLList = simqlTypeC[ListType, SIMQLList]
    implicit val funC: FunctionType \:> SIMQLFunction = simqlTypeC[FunctionType, SIMQLFunction]
    implicit val genC: Generics.type \:> Expr = simqlTypeC[Generics.type, Expr] // genericsは実質AnyなのでExprで型を潰す

    implicit val hnilC: Converter.AUX[HNil, HNil] = new Converter[HNil] {
      type O = HNil
      def convert(in: HNil, scope: Scope, ctx: QueryContext): Result[O] = {
        Right(HNil)
      }
    }
    implicit def hlistConverter[H, T <: HList, HR, TR <: HList](
      implicit
      hc: Converter.AUX[H, HR],
      tc: Converter.AUX[T, TR]
    ): Converter.AUX[H :: T, HR :: TR] = new Converter[H :: T] {
      type O = HR :: TR
      def convert(in: H :: T, scope: Scope, ctx: QueryContext): Result[O] = {
        val h :: t = in
        for {
          hr <- hc.convert(h, scope, ctx)
          tr <- tc.convert(t, scope, ctx)
        } yield hr :: tr
      }
    }
    trait ToList[T] {
      def toList(value: T): List[FunctionParam]
    }
    object ToList {
      implicit def simqlTypeTL[T <: SIMQLType] = new ToList[P[T]] {
        def toList(value: P[T]): List[FunctionParam] = List(value.toFunctionParam)
      }
      implicit val strTL = new ToList[P[StringType.type]] {
        def toList(value: P[StringType.type]): List[FunctionParam] = List(value.toFunctionParam)
      }
      implicit val hnilTL = new ToList[HNil] {
        def toList(value: HNil): List[FunctionParam] = Nil
      }
      implicit def hlistTL[H, T <: HList](implicit htl: ToList[H], ttl: ToList[T]) = new ToList[H :: T] {
        def toList(value: H :: T): List[FunctionParam] = {
          val h :: t = value
          htl.toList(h) ++ ttl.toList(t)
        }
      }
    }
  }

  private[this] def evalArgsFunction[Param <: HList, Arg](
    name: String,
    params: Param,
    retType: SIMQLType
  )(
    _apply: Arg => QueryContext => Result[Expr]
  )(
    implicit c: Converter.AUX[Param, Arg],
    tl: ToList[Param]
  ): SIMQLFunction = {
    val paramList = tl.toList(params)
    val impl = new BuildInFunction {
      override val key: String = name
      override val param: FunctionParam = paramList.last
      override val returnType: SIMQLType = retType

      override def apply0(scope: Scope, ctx: QueryContext, nextArgs: List[Value]): Result[Expr] =
        c.convert(params, scope, ctx).flatMap(_apply(_)(ctx))

      override def typeCheck(scope: Scope, paramMap: TypeMap): Result[SIMQLType] =
        Right(FunctionType(this.param.tpe, this.returnType))
    }
    paramList.init.foldRight[SIMQLFunction](impl) {
      case (p, next) =>
        val nextType = FunctionType(next.param.tpe, next.returnType)
        Closure(key = name, param = p, returnType = nextType, body = Nil, returnValue = next)
    }
  }

  object calc {
    private[this] def binaryArithmetic(name: String)(_apply: (BigDecimal, BigDecimal) => BigDecimal): SIMQLFunction =
      evalArgsFunction(name, "a" \> NumberType :: "b" \> NumberType :: HNil, NumberType) {
        args: NumberLit :: NumberLit :: HNil => _ =>
          val a :: b :: HNil = args
          Right(NumberLit(_apply(a.value, b.value)))
      }

    val Add = binaryArithmetic("add")(_ + _)
    val Sub = binaryArithmetic("sub")(_ - _)
    val Mul = binaryArithmetic("mul")(_ * _)
    val Div = binaryArithmetic("div") { (a, b) =>
      if (b != 0) a / b else 0
    }

    val ConcatString =
      evalArgsFunction(
        name = "cst",
        params = "a" \> StringType :: "b" \> StringType :: HNil,
        retType = StringType
      ) { args: StringLit :: StringLit :: HNil => _ =>
        val a :: b :: _ = args
        Right(StringLit(a.value + b.value))
      }

    val ConcatSymbol =
      evalArgsFunction(
        name = "csm",
        params = "a" \> SymbolType :: "b" \> SymbolType :: HNil,
        retType = SymbolType
      ) { args: SymbolLit :: SymbolLit :: HNil => _ =>
        val a :: b :: _ = args
        Right(SymbolLit(a.label + b.label))
      }

    val Eq = evalArgsFunction(
      name = "eq",
      params = "a" \> Generics :: "b" \> Generics :: HNil,
      retType = BooleanType
    ) { args: Expr :: Expr :: HNil => _ =>
      val a :: b :: _ = args
      Right(BooleanLit(a == b))
    }

    val Not = evalArgsFunction(
      name = "not",
      params = "bool" \> BooleanType :: HNil,
      retType = BooleanType
    ) { args: BooleanLit :: HNil => _ =>
      Right(BooleanLit(!args.head.value))
    }

    val values = List(Add, Sub, Mul, Div, ConcatString, ConcatSymbol, Eq, Not)
  }

  object list {
    val Cons = evalArgsFunction(
      name = "cons",
      params = "x" \> Generics :: "xs" \> ListType(Generics) :: HNil,
      retType = ListType(Generics)
    ) { args: Expr :: SIMQLList :: HNil => ctx =>
      val x :: xs :: _ = args
      val consed = xs.copy(elems = x :: xs.elems)
      Right(consed)
    }

    val values = List(Cons)
  }

  object control {
    val If = unevalArgsFunction(
      "if",
      List(FunctionParam("cond", BooleanType), FunctionParam("then", Generics), FunctionParam("else", Generics)),
      Generics
    ) { (scope, ctx) =>
      getArg[BooleanLit]("cond", scope, ctx).flatMap { cond =>
        if (cond.value) getArg[Expr]("then", scope, ctx)
        else getArg[Expr]("else", scope, ctx)
      }
    }

    val values = List(If)
  }
  object context {
    val GetTable =
      evalArgsFunction(name = "get_table", params = "index" \> NumberType :: HNil, retType = SymbolType) {
        args: NumberLit :: HNil => ctx =>
          val i = args.head.value
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
    val Debug = evalArgsFunction(name = "dbg", params = "value" \> Generics :: HNil, retType = Generics) {
      args: Expr :: HNil => _ =>
        val value = args.head
        println(value)
        Right(value)
    }

    val values = List(Debug)
  }
}
