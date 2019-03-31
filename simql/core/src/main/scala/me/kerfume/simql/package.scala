package me.kerfume

import me.kerfume.simql.node._

package object simql {
  type Scope = Map[String, SIMQLFunction]
  type Result[A] = Either[SIMQLError, A]
  trait SIMQLError
  sealed trait GlobalError extends SIMQLError
  case class FunctionNotFound(key: String) extends GlobalError
  case class FunctionParamTypeError(paramDef: FunctionParam, arg: Expr) extends GlobalError

  case class UnhandleError(msg: String) extends GlobalError

  // plane DTO
  case class QueryContext(tables: Vector[SymbolLit], globalScope: Scope, target: TranspileTarget) // need to global scope?
  object QueryContext {
    val empty = QueryContext(Vector.empty, Map.empty, TranspileTarget.MySQL)
  }
  sealed trait TranspileTarget
  object TranspileTarget {
    case object MySQL extends TranspileTarget
  }
}
