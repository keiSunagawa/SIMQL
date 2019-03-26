package me.kerfume.simql.smacro.func

import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.DefinitionNode.{ MacroFunc => FNode }
import me.kerfume.simql.node.QueryNode._
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.{ ASTMetaData, Result }
import me.kerfume.simql.functions._
import cats.instances.list._
import me.kerfume.simql.smacro.MacroFunc

object Generator {
  def generate(ast: FNode): Result[MacroFunc] = {
    val key = ast.symbol
    val params = ast.param

    for {
      ret <- ast.body.body.lastOption.collect { case Quasiquote(q) => q }.toRight("macro function body statements require quasiquote block on last.")
    } yield {
      ast.retType match {
        case RetSymbol =>
          new HighSymbolMacro(key, params, ret)
        case RetCond =>
          new CondMacro(key, params, ret)
      }
    }
  }
}

trait Checkable { self: MacroFunc =>
  protected[this] val typeCheck: (MacroParamType, MacroArg) => Result[Unit] = {
    case (StringType, m: StringWrapper)      => right
    case (NumberType, _: NumberWrapper)      => right
    case (SymbolType, _: SymbolWithAccessor) => right
    case (ExprType, _: Cond)                 => right
    case (p, a)                              => Left(s"function call error: arg type mismatch. param: $p, arg: $a, function name: $key")
  }
  protected[this] def resolveArgs(args: Seq[MacroArg]): Result[VarMap] = {
    if (parameters.length == args.length) {
      parameters
        .zip(args)
        .toList
        .mapE {
          case (p, a) =>
            typeCheck(p.tpe, a).map(_ => p.symbol -> a)
        }
        .map(_.toMap)
    } else {
      Left(s"function call error: unmatch args length. function name: $key")
    }
  }
}
