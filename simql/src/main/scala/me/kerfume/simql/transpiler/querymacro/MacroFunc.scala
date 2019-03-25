package me.kerfume.simql.transpiler.querymacro

import me.kerfume.simql.node.SimqlNode._
import me.kerfume.simql.node.DefinitionNode._
import me.kerfume.simql.transpiler.TranspileError

trait MacroFunc2 {
  val key: String
  type ReturnType

  val parameters: Seq[MacroParam]

  def apply(args: Seq[MacroArg]): Either[TranspileError, ReturnType] = {
    // args type check

    // apply body

    // return SimqlNode
    ???
  }
}

abstract class HighSymbolMacro2 extends MacroFunc2 {
  type ReturnType = HighSymbol
}

trait MacroFunc {
  val key: String
}

abstract class HighSymbolMacro extends MacroFunc {
  def apply0(args: Seq[SymbolWithAccessor]): Either[TranspileError, HighSymbol]
  def apply(args: Seq[MacroArg]): Either[TranspileError, HighSymbol] = {
    val validArgs = args.collect { case a: SymbolWithAccessor => a } // TODO allowed string and number
    for {
      _ <- Either.cond(validArgs.length == args.length, (), s"args type mismatch. macro function $$$key.")
      res <- apply0(validArgs)
    } yield res
  }
}

abstract class CondMacro extends MacroFunc {
  def apply0(
    symbolArgs: Seq[SymbolWithAccessor],
    stringArgs: Seq[StringWrapper],
    numberArgs: Seq[NumberWrapper]
  ): Either[TranspileError, Cond]
  def apply(args: Seq[MacroArg]): Either[TranspileError, Cond] = {
    val symbolArgs = args.collect { case a: SymbolWithAccessor => a }
    val stringArgs = args.collect { case a: StringWrapper      => a }
    val numberArgs = args.collect { case a: NumberWrapper      => a }
    val validArgs = symbolArgs ++ stringArgs ++ numberArgs
    for {
      _ <- Either.cond(validArgs.length == args.length, (), s"args type mismatch. macro function $$$key.")
      res <- apply0(symbolArgs, stringArgs, numberArgs)
    } yield res
  }
}

object MacroFunc {
  implicit class FuncsOps(fs: Seq[MacroFunc2]) {
    def highSymbolMacros: Map[String, HighSymbolMacro2] =
      fs.collect {
        case m: HighSymbolMacro2 =>
          m.key -> m
      }(collection.breakOut)
    def condMacros: Map[String, CondMacro] =
      fs.collect {
        case m: CondMacro =>
          m.key -> m
      }(collection.breakOut)
  }
}

object buildin {
  import me.kerfume.simql.transpiler.parser.Parser

  val countDefine: String = """defun c(col: Symbol) => Symbol = {
                              |  q{ $`COUNT(?)`($col()) }
                              |}""".stripMargin

  val cAst = Parser.parse(Parser.macroFunc, countDefine).get
  val Count = (for {
    f <-  MacroFuncGenerator.generate(cAst)
  } yield f).right.get

  case object Like extends CondMacro {
    val key: String = "like"
    def apply0(
      symbolArgs: Seq[SymbolWithAccessor],
      stringArgs: Seq[StringWrapper],
      numberArgs: Seq[NumberWrapper]
    ): Either[TranspileError, Cond] = {
      if (symbolArgs.length == 1 && stringArgs.length == 1 && numberArgs.isEmpty) {
        val col = symbolArgs.head
        val str = stringArgs.head
        val likeStr = str.copy(
          value = s"%${str.value}%"
        )
        Right(Raw(s"? LIKE(?)", List(col, likeStr)))
      } else Left(s"invalid args. macro function $$$key.")
    }
  }
  case object JoinExample extends CondMacro {
    val key = "je"
    def apply0(
      symbolArgs: Seq[SymbolWithAccessor],
      stringArgs: Seq[StringWrapper],
      numberArgs: Seq[NumberWrapper]
    ): Either[TranspileError, Cond] = {
      // ignore args
      Right(BinaryCond(BinaryOp(BinaryOp.EQ), SymbolWithAccessor(SymbolWrapper("id"), Some(Accessor(0))), SymbolWithAccessor(SymbolWrapper("author_id"), Some(Accessor(1)))))
    }
  }
}
