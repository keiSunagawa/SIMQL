package me.kerfume.simql

import cats.~>
import cats.Monad
import monix.execution.Scheduler.Implicits.global
import monix.execution.Cancelable
import monix.reactive._

import concurrent.duration._

trait Application[F[_]] {
  import Application._

  implicit def M: Monad[F]
  def interpreter: Runner.SimqlApp ~> F
  def predefCompiler: Compiler.Op ~> F
  def userdefCompiler: Compiler.Op ~> F
  def eventStream: Observable[Event]

  lazy val cancelable: Cancelable = eventStream.map {
    case Submit =>
      Runner.program.foldMap(interpreter)
    case PreDefCompile =>
      Runner.compile.foldMap(predefCompiler)
    case UserDefCompile =>
      Runner.compile.foldMap(userdefCompiler)
  }.subscribe()

  def start(): Cancelable = {
    cancelable
  }
  def cleanup() = {
    cancelable.cancel()
  }
}

object Application {
  sealed trait Event
  case object Submit extends Event
  case object PreDefCompile extends Event
  case object UserDefCompile extends Event
}
