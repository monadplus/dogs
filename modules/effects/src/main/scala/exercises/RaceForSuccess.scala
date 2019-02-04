package exercises

import cats.effect.{ExitCode, IOApp}

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
      _   <- timer.sleep { (300 + dur).millis }
      _   <- Sync[F].delay { if (Random.nextBoolean()) throw new Exception(s"$name") }
      txt <- Sync[F].delay { Random.alphanumeric.take(16).mkString }
    } yield Data(name, txt)

    proc.guaranteeCase {
      case ExitCase.Completed => Sync[F].delay { println(s"$name request finished") }
      case ExitCase.Canceled  => Sync[F].delay { println(s"$name request canceled") }
      case ExitCase.Error(ex) => Sync[F].delay { println(s"$name errored") }
    }
  }

  case class CompositeException(ex: NonEmptyList[Throwable]) extends Exception("All race candidates have failed") {
    def mkString: String = ex.map(_.getMessage).toList.mkString(", ")
  }

  object CompositeException {
    implicit val semigroup: Semigroup[CompositeException] = new Semigroup[CompositeException] {
      override def combine(x: CompositeException, y: CompositeException): CompositeException =
        CompositeException(x.ex ::: y.ex)
    }
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
  )(e: => CompositeException): M[A] =
    fiber.join.flatMap {
      case Right(resource) =>
        resource.pure[M]
      case Left(e2) =>
        (Left((e |+| toCompositeException(e2))): Either[Throwable, A]).pure[M].rethrow
    }

  def cancelWith[F[_]: Functor, A, B](fiber: Fiber[F, A])(b: => B): F[B] =
    fiber.cancel.map(_ => b)

  def raceToSuccess[T[_]: Reducible, F[_]: ConcurrentEffect, A](tfa: T[F[A]])(implicit ec: ExecutionContext): F[A] = {
    def parReduce(isActive: AtomicBoolean): F[A] =
      Reducible[T].reduceLeft(tfa) {
        case (l, r) =>
        if (!isActive.get) Concurrent[F].raiseError[A](new Exception("Cancelled"))
        else {
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
      }

    Concurrent[F].cancelable[A] { cb =>
      val isActive = new AtomicBoolean(true)

      ec.execute(() => {
        val res = parReduce(isActive).attempt.toIO.unsafeRunSync
        cb(res)
      })

      Sync[F].delay(isActive.set(false))
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

  override def run(args: List[String]): IO[ExitCode] = {
    val pool: ExecutorService =
      Executors.newCachedThreadPool()
    implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutorService(pool)

    for {
      fiber <- raceToSuccess(methods[IO])
                .map(data => Right(data))
                .handleErrorWith(e => IO(Left(toCompositeException(e))))
                .start
      // _ <- IO.sleep(150.millis)
      // _ <- fiber.cancel
      _ <- IO(pool.shutdown())
      data <- fiber.join
      _ <- report(data) 
    } yield ExitCode.Success
  }
}
