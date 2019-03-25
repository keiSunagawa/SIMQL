package me.kerfume.simql.transpiler.querymacro

import me.kerfume.simql.transpiler.ASTMetaData
import me.kerfume.simql.node.SimqlNode._
import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.node.DefinitionNode.{ MacroFunc => FNode }
import me.kerfume.simql.transpiler.{ TranspileError, ASTVisitor }
import me.kerfume.simql.transpiler.parser.Parser

import me.kerfume.simql.functions._
import cats.instances.list._

object MacroFuncGenerator {
  import Stubs._

  def generate(ast: FNode): Either[TranspileError, MacroFunc2] = {
    val key = ast.symbol
    val params = ast.param

    for {
      ret <- ast.body.body.lastOption.collect { case Quasiquote(q) => q }.toRight("error.")
    } yield {
      ast.retType match {
        case RetSymbol =>
          val retQuery = Parser.parse(Parser.highSymbol, ret).get // TODO safety
          generateHighSymbolMacro(key, params, retQuery)
        case RetCond =>
          Parser.parse(Parser.binaryCond, ret)
          ???
      }
    }
  }

  def generateHighSymbolMacro(fkey: String, fparams: Seq[MacroParam], ret: HighSymbol): HighSymbolMacro2 = new HighSymbolMacro2 {
    override val key = fkey
    override val parameters = fparams

    type VarMap = Map[String, MacroArg]

    val r = Right(())
    private[this] val typeCheck: (MacroParamType, MacroArg) => Either[TranspileError, Unit] = {
      case (StringType, _: StringWrapper) => r
      case (NumberType, _: NumberWrapper) => r
      case (SymbolType, _: SymbolWithAccessor) => r
      case _ => Left("type mismatch.")
    }
    private[this] def resolveArgs(args: Seq[MacroArg]): Either[TranspileError, VarMap] = {
      if (parameters.length == args.length) {
        parameters.zip(args).toList.mapE { case (p, a) =>
          typeCheck(p.tpe, a).map(_ => p.symbol -> a)
        }.map(_.toMap)
      } else {
        Left("unmatch args length.")
      }
    }
    override def apply(args: Seq[MacroArg]): Either[TranspileError, HighSymbol] = {
      for {
        varMap <- resolveArgs(args)
        visitor = macroApplyReplacer(varMap)
        ok <- visitor.visitHighSymbol(ret).run(ASTMetaData.empty)
      } yield ok
    }
  }

  def macroApplyReplacer(varMap: Map[String, MacroArg]): ASTVisitor = new ASTVisitor {
    import ASTVisitor._

    def resolve(node: MacroApply): Either[TranspileError, HighSymbol] = {
      varMap.get(node.symbol).toRight("not found variable.").flatMap { v =>
        v match {
          case hs: HighSymbol => Right(hs)
          case _ => Left("parameter type error.")
        }
      }
    }

    override def visitHighSymbol(node: HighSymbol): RE[HighSymbol] = node match {
      case m: MacroApply => re { _ => resolve(m) }
      case _ => super.visitHighSymbol(node)
    }
  }
}

object Stubs {
  val stubString = StringWrapper("")
  val stubNumber = NumberWrapper(0)
  val stubSymbol = SymbolWrapper("")
  val stubExpr = ???
}
