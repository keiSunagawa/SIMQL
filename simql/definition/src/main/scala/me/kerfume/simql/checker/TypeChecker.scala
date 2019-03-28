package me.kerfume.simql.checker

import me.kerfume.simql._
import me.kerfume.simql.node._
import me.kerfume.simql.functions._
import cats.instances.list._

class TypeChecker extends Checker {
  import TypeChecker._

  type ScopeValue = FunctionSignature
  def check(f: SIMQLFunction, scope: CScope): Result[CScope] = {
    val withParam: CScope = scope ++ f.params.map(p => p.key -> FunctionSignature(Nil, p.tpe))
    for {
      checked <- f.body.foldE(withParam) { case (acm, b) => checkBind(b, acm) }
      _ <- f.returnExpr match {
            case ff: FunctionCall =>
              for {
                ret <- checkFunctionCall(ff, checked)
                _ <- Either.cond(ret == f.returnType, (), ReturnTypeError(ret, f.returnType))
              } yield ()
            case other =>
              val ret = FunctionReturnType.fromExpr(other, Map.empty)
              Either.cond(ret == f.returnType, (), ReturnTypeError(ret, f.returnType))
          }
    } yield checked
  }

  private[this] def checkBind(b: Bind, scope: CScope): Result[CScope] = {
    def update(tpe: FunctionReturnType) = scope + (b.symbol -> FunctionSignature(Nil, tpe))
    b.value match {
      case f: FunctionCall =>
        checkFunctionCall(f, scope).map(update)
      case _ =>
        val tpe = FunctionReturnType.fromExpr(b.value, Map.empty)
        Right(update(tpe))
    }
  }
  private[this] def checkFunctionCall(f: FunctionCall, scope: CScope): Result[FunctionReturnType] = {
    val defun = scope(f.symbol)
    for {
      _ <- Either.cond(f.args.length == defun.param.length, (), UnmatchArgLength())
      _ <- f.args.zip(defun.param).zipWithIndex.mapE {
            case ((ff: FunctionCall, p), i) =>
              checkFunctionCall(ff, scope).flatMap { ret =>
                if (isSameType(ret, p)) Right(())
                else Left(TypeError(f.symbol, i, ff.symbol, p))
              }
            case ((a, p), i) =>
              val ret = FunctionReturnType.fromExpr(a, Map.empty)
              if (isSameType(ret, p)) Right(())
              else Left(TypeError(f.symbol, i, a.toString, p))
          }
    } yield defun.ret
  }

  private[this] def isSameType(arg: FunctionReturnType, param: FunctionParam): Boolean = {
    (arg, param) match {
      case (StringType, _: StringParam) => true
      case (NumberType, _: NumberParam) => true
      case (SymbolType, _: SymbolParam) => true
      case (_, _: ExprParam)            => true
      case _                            => false
    }
  }
}

object TypeChecker {
  case class TypeError(
    fname: String,
    index: Int,
    found: String,
    require: FunctionParam)
      extends SIMQLError
  case class ReturnTypeError(found: FunctionReturnType, require: FunctionReturnType) extends SIMQLError
  case class UnmatchArgLength() extends SIMQLError

  case class FunctionSignature(param: List[FunctionParam], ret: FunctionReturnType)
}
