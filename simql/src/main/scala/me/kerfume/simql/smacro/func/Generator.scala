package me.kerfume.simql.smacro.func

import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.DefinitionNode.{
  MacroFunc => FNode,
  StringWrapper => DString,
  NumberWrapper => DNumber,
  SymbolWrapper => DSymbol
}
import me.kerfume.simql.node.QueryNode.{StringWrapper => QString, NumberWrapper => QNumber, SymbolWrapper => QSymbol, _}
import me.kerfume.simql.parser.Parser
import me.kerfume.simql.{ASTMetaData, Result}
import me.kerfume.simql.functions._
import cats.instances.list._
import me.kerfume.simql.smacro.MacroFunc

object Generator {
  def generate(ast: FNode, fmap: Map[String, InnerFunc]): Result[MacroFunc] = {
    val key = ast.symbol
    val params = ast.param

    for {
      ret <- ast.body.body.lastOption.collect { case Quasiquote(q) => q }
              .toRight("macro function body statements require quasiquote block on last.")
    } yield {
      ast.retType match {
        case RetSymbol =>
          new HighSymbolMacro(key, params, ast.body, fmap)
        case RetCond =>
          new CondMacro(key, params, ast.body, fmap)
      }
    }
  }
}

trait Checkable { self: MacroFunc =>
  protected[this] val typeCheck: (MacroParamType, MacroArg) => Result[Unit] = {
    case (StringType, m: QString)            => right
    case (NumberType, _: QNumber)            => right
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

  protected[this] def returnQueryParse[RetutnType](
    last: MacroStatement,
    parser: Parser.Parser[RetutnType]
  ): Result[RetutnType] = {
    for {
      query <- last match {
                case Quasiquote(q) => Right(q)
                case _             => Left("macro function body statements require quasiquote block on last.")
              }
      ret <- Parser
              .parse(parser, query)
              .map(Right(_))
              .getOrElse(Left(s"function call error: parse failed return value. function name: $key"))
    } yield ret
  }

  def qNodeToDNode(from: VarMap): Map[String, Value] = {
    from.flatMap {
      case (key, value) =>
        PartialFunction.condOpt(value) {
          case n: QString            => key -> DString(n.value)
          case n: QNumber            => key -> DNumber(n.value)
          case n: SymbolWithAccessor => key -> DSymbol(n.symbol.label)
        }
    }
  }
  def dNodeToQNode(from: Map[String, Value]): VarMap = {
    from.flatMap {
      case (key, value) =>
        PartialFunction.condOpt(value) {
          case n: DString => key -> QString(n.value)
          case n: DNumber => key -> QNumber(n.value)
          case n: DSymbol => key -> QSymbol(n.label) // TODO convert to with accessor
        }
    }
  }
}
