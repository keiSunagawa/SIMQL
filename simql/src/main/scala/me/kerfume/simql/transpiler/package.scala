package me.kerfume.simql

import me.kerfume.simql.node.SimqlNode._
import me.kerfume.simql.transpiler.querymacro._

package object transpiler {
  type TranspileError = String
  // ただのDTOであるべき...?
  case class ASTMetaData(tables: Seq[SymbolWrapper], macroFuncs: Seq[MacroFunc2] = Nil)
  object ASTMetaData {
    val empty = ASTMetaData(Nil)
  }
}
