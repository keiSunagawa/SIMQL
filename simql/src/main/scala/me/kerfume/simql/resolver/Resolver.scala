package me.kerfume.simql.resolver

import me.kerfume.simql._
import me.kerfume.simql.node.QueryNode._

trait Resolver {
  def resolve(ast: Query, meta: ASTMetaData): Result[Query]
}
