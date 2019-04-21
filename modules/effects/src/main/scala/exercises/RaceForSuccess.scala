package exercises

import cats.effect.IOApp

// Requirements:
// 1.- The function should run requests in parallel.
// 2.- The function should wait for the first request to complete successfuly.
// 3.- Once a first request has completed, everything that is still in-flight must be cancelled.
// 4.- If all requests have failed, all errors should be reported for better debugging.

object RaceForSuccess extends IOApp {
  import cats._, cats.data._, cats.implicits._
  import cats.effect._, cats.effect.implicits._, cats.effect.concurrent._

  import scala.util.Random
  import scala.concurrent.duration._
  import scala.util._
  import java.util.concurrent.atomic._
  import scala.concurrent._ // ExecutionContext
  import java.util.concurrent.{ExecutorService, Executors}

  case class Data(source: String, body: String)

  def provider[F[_]: Sync](name: String)(implicit timer: Timer[F]): F[Data] = {
    val proc = for {
      dur <- Sync[F].delay { Random.nextInt(500) }
      _ <- timer.sleep { (100 + dur).millis }
      _ <- Sync[F].delay { if (Random.nextBoolean()) throw new Exception(s"$name") }
      txt <- Sync[F].delay { Random.alphanumeric.take(16).mkString }
    } yield Data(name, txt)

    proc.guaranteeCase {
      case ExitCase.Completed => Sync[F].delay { println(s"$name request finished") }
      case ExitCase.Canceled  => Sync[F].delay { println(s"$name request canceled") }
      case ExitCase.Error(ex) => Sync[F].delay { println(s"$name errored") }
    }
  }

  // final class CompositeException(val head: Throwable, val tail: NonEmptyList[Throwable])
  //  extends RuntimeException(
  //    s"Multiple exceptions were thrown (${1 + tail.size}), " +
  //    s"first ${head.getClass.getName}: ${head.getMessage}")
  //    with Serializable {
  //
  //  /** Returns the set of all errors wrapped by this composite. */
  //  def all: NonEmptyList[Throwable] =
  //    head :: tail
  //}
  case class CompositeException(ex: NonEmptyList[Throwable])
      extends RuntimeException("All race candidates have failed") with Serializable {
    def mkString: String = ex.map(_.getMessage).toList.mkString(", ")
    def and(e: Throwable): CompositeException = CompositeException(ex :+ e)
  }

  def toCompositeException(e: Throwable): CompositeException =
    e match {
      case compositeException: CompositeException =>
        compositeException
      case _ =>
        CompositeException(NonEmptyList.one(e))
    }

  def waitResultOrCombineError[M[_]: MonadError[?[_], Throwable], A](
      fiber: Fiber[M, Either[Throwable, A]]
  )(ce: => CompositeException): M[A] =
    fiber.join.flatMap {
      case Right(resource) =>
        resource.pure[M]
      case Left(e) =>
        (ce.and(e): Throwable).raiseError[M, A]
    }

  def cancelWith[F[_]: Functor, A, B](fiber: Fiber[F, A])(b: => B): F[B] =
    fiber.cancel.as(b)

  def raceToSuccess[T[_]: Reducible, F[_]: Concurrent, A](tfa: T[F[A]]): F[A] =
    Reducible[T].reduceLeft(tfa) {
      case (l, r) =>
        Concurrent[F].racePair(l.attempt, r.attempt).flatMap {
          case Left((Right(resource), r)) =>
            cancelWith(r)(resource)
          case Right((l, Right(resource))) =>
            cancelWith(l)(resource)
          case Left((Left(e), r)) =>
            waitResultOrCombineError(r)(toCompositeException(e))
          case Right((l, Left(e))) =>
            waitResultOrCombineError(l)(toCompositeException(e))
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

  def report(value: Either[CompositeException, Data]): IO[Unit] =
    value match {
      case Left(e) =>
        IO(println("Resources that have failed: " + e.mkString))
      case Right(Data(source, content)) =>
        IO(println(s"$source finished with content: $content"))
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      fiber <- raceToSuccess(methods[IO])
        .redeem(toCompositeException(_).asLeft, _.asRight)
        .start
      data <- fiber.join
      _ <- report(data)
    } yield ExitCode.Success
}
