import cats._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Deferred
import cats.effect.implicits._
import scala.concurrent.duration._

object Await extends IOApp {
  def await[F[_]: Concurrent, A](fa: F[A]): F[(F[Option[Either[Throwable, A]]], CancelToken[F])] =
    Deferred[F, Option[Either[Throwable, A]]].flatMap { result =>
      val action = fa.attempt.flatMap { r =>
        result.complete(r.some).uncancelable
      }.guaranteeCase {
        // fiber.cancel triggers `ExitCase.Canceled` even the IO has executed once
        case ExitCase.Canceled => result.complete(None).attempt.void
        case _ => ().pure[F]
      }

      action.start.bracketCase( fiber =>
        (result.get -> fiber.cancel).pure[F]
      ) {
        case (fiber, ExitCase.Canceled) => fiber.cancel
        case (_, _) => ().pure[F]
      }
    }

  def cancelable[F[_]: Sync](implicit timer: Timer[F]): F[Int] =
    timer.sleep(2.seconds) *> 10.pure[F]

  override def run(args: List[String]): IO[ExitCode] =
    for {
      // The cool part of await is that when executed, starts computing the F[A] and there is no need to call result
      // result will semantically block until either the result is computed or the computation is canceled
      (result, cancelToken) <- await(cancelable[IO])
      _ <- cancelToken
      r <- result
      _ <- IO(println(s"Result $r"))
    } yield ExitCode.Success
}