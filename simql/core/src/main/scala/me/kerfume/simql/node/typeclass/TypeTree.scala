package me.kerfume.simql.node.typeclass

import cats.instances.list._
import me.kerfume.simql._
import me.kerfume.simql.functions._
import me.kerfume.simql.node.SIMQLFunction.TypeMap
import me.kerfume.simql.node._
import simulacrum._
import TypeTree._

@typeclass trait ToTypeTree[A] {
  def toTree(tpe: A): TypeTree
}

object ToTypeTree {
  def instance[A](f: A => TypeTree): ToTypeTree[A] = new ToTypeTree[A] {
    override def toTree(a: A): TypeTree = f(a)
  }
  implicit val elemTTT: ToTypeTree[ElemType] = instance { Has0(_) }
  implicit val genericsTTT: ToTypeTree[Generics] = instance { Has0(_) }
  implicit val listTTT: ToTypeTree[ListType] = instance {
    case ListType(t) => Has1(ListC, ToTypeTree[SIMQLType].toTree(t))
  }
  implicit val funcTTT: ToTypeTree[FunctionType] = instance {
    case FunctionType(t1, t2) => Has2(FuncC, ToTypeTree[SIMQLType].toTree(t1), ToTypeTree[SIMQLType].toTree(t2))
  }

  implicit val allTTT: ToTypeTree[SIMQLType] = instance {
    case l: ListType     => listTTT.toTree(l)
    case f: FunctionType => funcTTT.toTree(f)
    case g: Generics     => genericsTTT.toTree(g)
    case e: ElemType     => elemTTT.toTree(e)
  }
}

sealed trait TypeTree {
  def toType: SIMQLType
}

object TypeTree {
  case class Has0(tpe: SIMQLType) extends TypeTree {
    def toType = tpe
  }
  case class Has1(constructor: Has1Constructor, tpe: TypeTree) extends TypeTree {
    def toType = constructor match {
      case ListC => ListType(tpe.toType)
    }
  }
  case class Has2(constructor: Has2Constructor, tpe1: TypeTree, tpe2: TypeTree) extends TypeTree {
    def toType = constructor match {
      case FuncC => FunctionType(tpe1.toType, tpe2.toType)
    }
  }

  sealed trait Has1Constructor
  case object ListC extends Has1Constructor
  sealed trait Has2Constructor
  case object FuncC extends Has2Constructor
}
