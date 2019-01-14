//package exercises
//
//import scala.util.Random
//import scala.concurrent.duration._
////import cats._
//import cats.implicits._
//import cats.effect.{ExitCode, IO, IOApp, Timer}
//import cats.effect.concurrent.Ref
//
//object WorkerPool extends IOApp {
//
//  // To start, our requests can be modelled as simple functions.
//  // You might want to replace this type with a class if you go for bonuses. Or not.
////  type Worker[A, B] = A => IO[B]
//
//  final case class Worker[F[_], A, B](run: A => F[B], id: Int)
//
//  // Sample stateful worker that keeps count of requests it has accepted
//  def mkWorker(id: Int)(implicit timer: Timer[IO]): IO[Worker[Int, Int]] =
//    Ref[IO].of(0).map { counter =>
//      def simulateWork: IO[Unit] =
//        IO(50 + Random.nextInt(450)).map(_.millis).flatMap(IO.sleep)
//
//      def report: IO[Unit] =
//        counter.get.flatMap(i => IO(println(s"Total processed by $id: $i")))
//
//      x =>
//        simulateWork >>
//          counter.update(_ + 1) >>
//          report >>
//          IO.pure(x + 1)
//    }
//
//  trait WorkerPool[A, B] {
//    def exec(a: A): IO[B]
//  }
//
//  getWorker with an Ref that increments
//  getNWorkers
//  object WorkerPool {
//    // Implement this constructor, and, correspondingly, the interface above.
//    // You are free to use named or anonymous classes
//    def of[A, B](fs: List[Worker[A, B]]): IO[WorkerPool[A, B]] = ???
//  }
//
//  def testPool(ids: Ref[IO, Int]): IO[WorkerPool[IO, Int, Int]] = for {
//    workers <- getNWorkers(5, ids)
//    pool <- WorkerPool.of[IO, Int, Int](NonEmptyList.fromListUnsafe(workers))
//  } yield pool
//
//
////  - Processing jobs must run in parallel
////  - Submitting a processing request must wait if all workers are busy.
////  - Submission should do load balancing: wait for the first worker to finish, not for a certain one.
////  - Worker should become available whenever a job is completed successfully, with an exception or cancelled.
//
//  override def run(args: List[String]): IO[ExitCode] =
//    for {
//      _ <- IO.pure("")
//    } yield ExitCode.Success
//}
//
//// Hints:
////  - Relying on a concurrent queue might be a good idea. And MVar is essentially a one-element queue.
////  - Because our workers are functions of type A => IO[B], we can freely do anything effectful before and after running function.
////  - Our factory method (apply on companion) returns IO too. This lets us create a shared MVar and do pre-initialization, if needed.
