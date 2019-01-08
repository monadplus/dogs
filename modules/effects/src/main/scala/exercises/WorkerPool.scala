package exercises

import scala.util.Random
import scala.concurrent.duration._
//import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Timer}
import cats.effect.concurrent.Ref

object WorkerPool extends IOApp {

  // To start, our requests can be modelled as simple functions.
  // You might want to replace this type with a class if you go for bonuses. Or not.
  type Worker[A, B] = A => IO[B]

  // Sample stateful worker that keeps count of requests it has accepted
  def mkWorker(id: Int)(implicit timer: Timer[IO]): IO[Worker[Int, Int]] =
    Ref[IO].of(0).map { counter =>
      def simulateWork: IO[Unit] =
        IO(50 + Random.nextInt(450)).map(_.millis).flatMap(IO.sleep)

      def report: IO[Unit] =
        counter.get.flatMap(i => IO(println(s"Total processed by $id: $i")))

      x =>
        simulateWork >>
          counter.update(_ + 1) >>
          report >>
          IO.pure(x + 1)
    }

  trait WorkerPool[A, B] {
    def exec(a: A): IO[B]
  }

  object WorkerPool {
    // Implement this constructor, and, correspondingly, the interface above.
    // You are free to use named or anonymous classes
    def of[A, B](fs: List[Worker[A, B]]): IO[WorkerPool[A, B]] = ???
  }

  // Sample test pool to play with in IOApp
  val testPool: IO[WorkerPool[Int, Int]] =
    List
      .range(0, 10)
      .traverse(mkWorker)
      .flatMap(WorkerPool.of)

  override def run(args: List[String]): IO[ExitCode] = ???
//    for {
//      _ <- IO.pure(???)
//    } yield ExitCode.Success
}
