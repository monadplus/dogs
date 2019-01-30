package exercises

import cats.effect._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Future2IOAsync {
  // IO.fromFuture
  def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    fa.value match {
      case Some(result) =>
        result match {
          case Success(value) => IO.pure(value)
          case Failure(e)     => IO.raiseError(e)
        }
      case None =>
        IO.async { cb =>
          fa.onComplete {
            case Success(value) => cb(Right(value))
            case Failure(e)     => cb(Left(e))
          }
        }
    }
}
