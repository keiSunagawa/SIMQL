package me.kerfume.simql.defun

import me.kerfume.simql
import me.kerfume.simql.{Result, Scope}
import me.kerfume.simql.node._

object buildin {
  object Add extends SIMQLFunction {
    val key: String = "add"
    val params: List[FunctionParam] = List(NumberParam("num1"), NumberParam("num2"))
    val returnType: FunctionReturnType = NumberType

    val body: List[Bind] = Nil
    val returnExpr: Expr = FunctionCall("result", Nil)

    override def apply0(scope: Scope, meta: simql.ASTMetaData): Result[Expr] = {
      val n1 = scope("num1")(Nil, scope, meta).right.get.asInstanceOf[NumberLit].value
      val n2 = scope("num2")(Nil, scope, meta).right.get.asInstanceOf[NumberLit].value
      Right(NumberLit(n1 + n2))
    }
  }

  val functions: Scope = List(Add).map(f => f.key -> f)(collection.breakOut)
}
