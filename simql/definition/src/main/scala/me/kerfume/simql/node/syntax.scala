package me.kerfume.simql.node

object syntax {
  sealed trait FType
  case object StringType extends FType
  case object NumberType extends FType
  case object SymbolType extends FType
  case object RawType extends FType
  case object ExprType extends FType
}
