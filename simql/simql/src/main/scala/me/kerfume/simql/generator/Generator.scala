package me.kerfume.simql.generator

import me.kerfume.simql.node._

trait Generator {
  type Code
  def generate(ast: Query): Code
}
