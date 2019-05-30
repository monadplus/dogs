package concurrency

import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.concurrent._
import concurrency.PurePromise.Callback

import scala.util.{Either, Left, Right}

object PurePromise {
  type Callback[-A] = Either[Throwable, A] => Unit
}

class PurePromise[F[_], A](ref: Ref[F, Either[List[Callback[A]], A]])(implicit F: Async[F]) {
  def get: F[A] = F.asyncF { cb =>
    ref.modify {
      case r @ Right(value) =>
        r -> F.delay(cb(Right(value)))
      case Left(waiting) =>
        Left(cb :: waiting) -> F.unit
    }.flatten
  }

  def complete(value: A): F[Unit] =
    ref.modify {
      case current @ Right(_) =>
        (current, ().pure[F])
      case Left(waiting) =>
        (Right(value), Sync[F].delay(waiting.foreach(cb => cb(Right(value)))))
    }.flatten
}

class PurePromise2[F[_]: Concurrent, A](w: MVar[F, A]) {
  def get: F[A] =
    w.read
  def complete(value: A): F[Unit] =
    w.tryPut(value).void
}
