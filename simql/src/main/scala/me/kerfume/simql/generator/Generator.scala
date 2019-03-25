package me.kerfume.simql.generator

import me.kerfume.simql.node.QueryNode._

trait Generator {
  type Code
  def generate(ast: Query): Code
}
