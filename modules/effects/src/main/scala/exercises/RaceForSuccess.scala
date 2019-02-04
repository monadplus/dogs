package exercises

import cats.effect.{ExitCode, IOApp}

// Requirements:
// 1.- The function should run requests in parallel.
// 2.- The function should wait for the first request to complete successfuly.
// 3.- Once a first request has completed, everything that is still in-flight must be cancelled.
// 4.- If all requests have failed, all errors should be reported for better debugging.

// Bonus:
// B1.- Avoid using runtime checking for CompositeException (including pattern matching on it).
// B2.- If returned IO is cancelled, all in-flight requests should be properly cancelled as well.

object RaceForSuccess extends IOApp {
  import cats._, cats.data._, cats.implicits._
  import cats.effect._, cats.effect.implicits._, cats.effect.concurrent._

  import scala.util.Random
  import scala.concurrent.duration._
  import scala.util._

  case class Data(source: String, body: String)

  def provider[F[_]: Sync](name: String)(implicit timer: Timer[F]): F[Data] = {
    val proc = for {
      dur <- Sync[F].delay { Random.nextInt(500) }
      _   <- timer.sleep { (100 + dur).millis }
      _   <- Sync[F].delay { if (Random.nextBoolean()) throw new Exception(s"$name") }
      txt <- Sync[F].delay { Random.alphanumeric.take(16).mkString }
    } yield Data(name, txt)

    proc.guaranteeCase {
      case ExitCase.Completed => Sync[F].delay { println(s"$name request finished") }
      case ExitCase.Canceled  => Sync[F].delay { println(s"$name request canceled") }
      case ExitCase.Error(ex) => Sync[F].delay { println(s"$name errored") }
    }
  }

  case class CompositeException[R[_]: Reducible: Functor](ex: R[Throwable]) extends Exception("All race candidates have failed") {
    def mkString: String = ex.map(_.getMessage).reduceLeft(_ + "," + _)
  }

  object CompositeException {
    implicit def semigroup[R[_]: Reducible: Functor](implicit ev: Semigroup[R[Throwable]]): Semigroup[CompositeException[R]] = new Semigroup[CompositeException[R]] {
      override def combine(x: CompositeException[R], y: CompositeException[R]): CompositeException[R] =
        CompositeException[R](x.ex |+| y.ex)
    }
  }

  def toCompositeException[R[_]: Reducible : Applicative](e: Throwable): CompositeException[R] =
    e match {
      case ce@CompositeException(_) =>
        ce.asInstanceOf[CompositeException[R]]
      case _ =>
        CompositeException[R](Applicative[R].pure(e))
    }

  def waitResultOrCombineError[M[_]: MonadError[?[_], Throwable], R[_]: Applicative : Reducible, A](
    fiber: Fiber[M, Either[Throwable, A]]
  )(e: => CompositeException[R])(implicit ev: Semigroup[R[Throwable]]): M[A] =
    fiber.join.flatMap {
      case Right(resource) =>
        resource.pure[M]
      case Left(e2) =>
        (Left((e |+| toCompositeException[R](e2))): Either[Throwable, A]).pure[M].rethrow
    }

  def cancelWith[F[_]: Functor, A, B](fiber: Fiber[F, A])(b: => B): F[B] =
    fiber.cancel.map(_ => b)

  def raceToSuccess[R[_]: Applicative: Reducible, F[_]: Concurrent, A](tfa: R[F[A]])(implicit ev: Semigroup[R[Throwable]]): F[A] =
    Reducible[R].reduceLeft(tfa) {
      case (l, r) =>
        Concurrent[F].racePair(l.attempt, r.attempt).flatMap {
          case Left((Right(resource), r)) =>
            cancelWith(r)(resource)
          case Right((l, Right(resource))) =>
            cancelWith(l)(resource)
          case Left((Left(e), r)) =>
            waitResultOrCombineError(r)(toCompositeException[R](e))
          case Right((l, Left(e))) =>
            waitResultOrCombineError(l)(toCompositeException[R](e))
        }
    }

  def methods[F[_]: Sync: Timer]: NonEmptyList[F[Data]] =
    NonEmptyList
      .of(
        "memcached",
        "redis",
        "postgres",
        "mongodb",
        "hdd",
        "aws"
      )
      .map(provider[F])

  def report(value: Either[CompositeException[NonEmptyList], Data]): IO[Unit] =
    value match {
      case Left(e) =>
        IO(println("Resources that have failed: " + e.mkString))
      case Right(Data(source, content)) =>
        IO(println(s"$source finished with content: $content"))
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      dataOrError <- raceToSuccess(methods[IO])
                      .map(data => Right(data))
                      .handleErrorWith(e => IO(Left(toCompositeException[NonEmptyList](e))))

      _ <- report(dataOrError)

    } yield ExitCode.Success
}
