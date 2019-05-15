package exercises

import cats._, cats.data._, cats.implicits._
import cats.effect._, cats.effect.implicits._, cats.effect.concurrent._
import fs2._

import scala.util.Random
import scala.concurrent.duration._
import scala.util._
import java.util.concurrent.atomic._
import scala.concurrent._ // ExecutionContext
import java.util.concurrent.{ExecutorService, Executors}

trait TokenBucket[F[_]] {
  def use[A](fa: F[A]): F[A]
  def available: F[Long]
}

object TokenBucket {
  def apply[F[_]: Concurrent: Timer](permits: Long, interval: FiniteDuration): F[TokenBucket[F]] =
    concurrentTokenBucket[F](permits, interval)

  def concurrentTokenBucket[F[_]: Concurrent: Timer](
      permits: Long,
      interval: FiniteDuration
  ): F[TokenBucket[F]] =
    Semaphore[F](permits).flatMap { semaphore =>
      // Race condition here.
      val tryRelease: F[Unit] =
        semaphore.available.map(_ >= permits).ifM(().pure[F], semaphore.release)

      val releaseEvery: F[Unit] =
        Stream
          .awakeEvery[F](interval)
          .evalMap { _ =>
            tryRelease
          }
          .compile
          .drain
          .start
          .void

      releaseEvery.map { _ =>
        new TokenBucket[F] {
          def use[A](fa: F[A]): F[A] =
            semaphore.acquire >> fa
          def available: F[Long] =
            semaphore.available
        }
      }
    }
}

object TokenBucketTest extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    TokenBucket[IO](5, 1.seconds)
      .flatMap { bucket =>
        val showPermits = bucket.available.flatMap(c => IO(println(s"available: $c")))

        for {
          _ <- showPermits
          // Should block until a token is addded.
          _ <- bucket.use(IO.unit).replicateA(6)
        } yield ()
      }
      .as(ExitCode.Success)
}
