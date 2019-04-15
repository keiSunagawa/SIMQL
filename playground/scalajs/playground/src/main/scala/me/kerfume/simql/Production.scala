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
    sendSQL: js.Function1[String, Unit]
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
    val app = new ApplicationProd(presenter or rdb)
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
  @JSExport // TODO 必要かな...?
  def submit(): Unit = {
    onNext(Application.Submit)
  }
  @JSExport
  def complete(): Unit = {
    app.cleanup()
    onComplete()
  }
}
// TODO interpreter convert to IO Monad
class ApplicationProd(val interpreter: Runner.SimqlApp ~> Id) extends Application[Id] {
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
