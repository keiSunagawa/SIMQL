package me.kerfume.simql

import cats._
import cats.data.NonEmptyList
import cats.syntax.semigroupk._

import scala.language.higherKinds

object functions {
  def transpose[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] = opt match {
    case Some(either) => either.map(Some(_))
    case None         => Right(None)
  }

  implicit class FoldableOps[A, B, F[_]: Foldable: MonoidK: Applicative](e: F[A]) {
    def mapE[E, B](f: A => Either[E, B]): Either[E, F[B]] = {
      Foldable[F].foldLeft[A, Either[E, F[B]]](e, Right(MonoidK[F].empty)) {
        case (before, x) =>
          before match {
            case Right(xs) => f(x).map(x2 => xs <+> Applicative[F].pure(x2))
            case Left(e)   => Left(e)
          }
      }
    }
    def foldE[E, B](init: B)(f: (B, A) => Either[E, B]): Either[E, B] = {
      Foldable[F].foldLeft[A, Either[E, B]](e, Right(init)) {
        case (before, x) =>
          before match {
            case Right(acm) => f(acm, x)
            case l          => l
          }
      }
    }
  }
  implicit class PipelineOps[T](x: T) {
    def |>[S](f: T => S): S = f(x)
  }
}

object types {
  type NEL[+A] = NonEmptyList[A]
}
