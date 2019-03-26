package me.kerfume

import me.kerfume.simql.node.QueryNode.SymbolWrapper
import me.kerfume.simql.smacro.MacroFunc

package object simql {
  type SimqlError = String
  type Result[A] = Either[SimqlError, A]

  // ただのDTOであるべき
  case class ASTMetaData(tables: Seq[SymbolWrapper], macroFuncs: Seq[MacroFunc])
  object ASTMetaData {
    val empty = ASTMetaData(Nil, Nil)
  }
}
