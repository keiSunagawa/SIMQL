package me.kerfume

import me.kerfume.simql.node._
import me.kerfume.simql.node.SIMQLFunction._

package object simql {
  type Scope = Map[String, Value]
  type Result[A] = Either[SIMQLError, A]
  trait SIMQLError
  sealed trait GlobalError extends SIMQLError
  case class FunctionNotFound(key: String) extends GlobalError

  case class UnhandleError(msg: String) extends GlobalError

  // plane DTO
  case class QueryContext(
    tables: Vector[SymbolLit],
    globalScope: Scope,
    target: TranspileTarget,
    typeMap: Map[String, SIMQLType])
  object QueryContext {
    val empty = QueryContext(Vector.empty, Map.empty, TranspileTarget.MySQL, Map.empty)
  }
  sealed trait TranspileTarget
  object TranspileTarget {
    case object MySQL extends TranspileTarget
  }
}
