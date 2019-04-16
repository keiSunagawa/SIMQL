package me.kerfume.simql

import cats.{~>, Id, InjectK, Monad}
import cats.Monad
import cats.arrow.FunctionK
import cats.catsInstancesForId
import monix.execution.Cancelable
import monix.reactive._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SIMQL")
object Production {
  @JSExport
  def compile(
    getSimqlQuery: js.Function0[String],
    getPredef: js.Function0[String],
    getUserdef: js.Function0[String],
    printSQL: js.Function1[String, Unit],
    printError: js.Function1[String, Unit],
    sendSQL: js.Function1[String, Unit],
    // predef
    predefGet: js.Function0[js.Array[String]],
    predefError: js.Function1[String, Unit],
    predefSetComplition: js.Function1[js.Array[String], Unit],
    // userdef
    userdefGet: js.Function0[js.Array[String]],
    userdefError: js.Function1[String, Unit],
    userdefSetComplition: js.Function1[js.Array[String], Unit],
  ): EventStreamHandler = {
    val presenter = new FunctionK[Presenter.Op, Id] {
      import Presenter._
      def apply[A](op: Op[A]) = op match {
        case GetSimqlQuery     => getSimqlQuery.apply()
        case GetPredef     => getPredef.apply()
        case GetUserdef     => getUserdef.apply()
        case PrintSQL(sql)     => printSQL.apply(sql)
        case PrintError(error) => printError.apply(error)
      }
    }

    val rdb = new FunctionK[RDB.Op, Id] {
      import RDB._
      def apply[A](op: Op[A]) = op match {
        case SendSQL(sql) =>
          sendSQL.apply(sql)
      }
    }

    import js.JSConverters._
    val predefCompiler = new FunctionK[Compiler.Op, Id] {
      import Compiler._
      def apply[A](op: Op[A]) = op match {
        case GetDef => predefGet.apply().toList
        case PrintError(error) => predefError.apply(error)
        case SetCompletion(funcNames) => predefSetComplition.apply(funcNames.toJSArray)
      }
    }
    val userdefCompiler = new FunctionK[Compiler.Op, Id] {
      import Compiler._
      def apply[A](op: Op[A]) = op match {
        case GetDef => userdefGet.apply().toList
        case PrintError(error) => userdefError.apply(error)
        case SetCompletion(funcNames) => userdefSetComplition.apply(funcNames.toJSArray)
      }
    }
    val app = new ApplicationProd(presenter or rdb, predefCompiler, userdefCompiler)
    // 重要
    app.start()
    app.handler
  }
}

class EventStreamHandler(
  onNext: Application.Event => Unit,
  onComplete: () => Unit,
  app: Application[Id] // u-n...
) {
  @JSExport
  def submit(): Unit = {
    onNext(Application.Submit)
  }
  @JSExport
  def predefCompile(): Unit = {
    onNext(Application.PreDefCompile)
  }
  @JSExport
  def userdefCompile(): Unit = {
    onNext(Application.UserDefCompile)
  }
  @JSExport
  def complete(): Unit = {
    app.cleanup()
    onComplete()
  }
}
// TODO interpreter convert to IO Monad
class ApplicationProd(
  val interpreter: Runner.SimqlApp ~> Id,
  val predefCompiler: Compiler.Op ~> Id,
  val userdefCompiler: Compiler.Op ~> Id
) extends Application[Id] {
  implicit val M: Monad[Id] = catsInstancesForId
  var handler: EventStreamHandler = null
  val eventStream = Observable.unsafeCreate[Application.Event] { s =>
    val h = new EventStreamHandler(
      event => s.onNext(event),
      () => s.onComplete(),
      this
    )
    handler = h
    Cancelable(() => ())
  }
}
