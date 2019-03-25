package me.kerfume.simql.transpiler

import me.kerfume.simql.node.SimqlNode.SimqlRoot

trait Checker {
  def check(ast: SimqlRoot): Either[String, Unit]
}
