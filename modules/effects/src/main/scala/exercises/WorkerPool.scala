package exercises

import cats.effect.{ExitCode, IOApp}

//   Requirements:
//   1. Processing jobs must run in parallel
//   2. Submitting a processing request must wait if all workers are busy.
//   3. Submission should do load balancing: wait for the first worker to finish, not for a certain one.
//   4. Worker should become available whenever a job is completed successfully, with an exception or cancelled.

//   Bonus:
//   B1. Generalize for any F using Concurrent typeclass.
//   B2. Add methods to WorkerPool interface for adding workers on the fly and removing all workers.
//       If all workers are removed, submitted jobs must wait until one is added.

object WorkerPoolExercise extends IOApp {
  import cats._, cats.data._, cats.implicits._
  import cats.effect._, cats.effect.implicits._, cats.effect.concurrent._

  import scala.util.Random
  import scala.concurrent.duration._
  import scala.util._

  final case class Worker[F[_], A, B](run: A => F[B])

  def mkWorker[F[_]: Concurrent](id: Int)(implicit timer: Timer[F]): F[Worker[F, Int, Int]] =
    Ref[F].of(0).map { counter =>
      def simulateWork: F[Unit] =
        Sync[F].delay(50 + Random.nextInt(450)).map(_.millis).flatMap(timer.sleep)

      def report: F[Unit] =
        counter.get.flatMap(i => Sync[F].delay(println(s"Total processed by $id: $i")))

      val f: Int => F[Int] =
        x =>
          simulateWork >>
            counter.update(_ + 1) >>
            report >>
            Applicative[F].pure(x + 1)

      Worker(f)
    }

  trait WorkerPool[F[_], A, B] {
    def exec(a: A): F[B]
    def addWorker(w: Worker[F, A, B]): F[Unit]
    def removeAllWorkers: F[Unit]
  }

  object WorkerPool {
    def of[F[_]: Concurrent, A, B](fs: List[Worker[F, A, B]]): F[WorkerPool[F, A, B]] =
      for {
        workersQueue <- MVar[F].empty[Worker[F, A, B]]
        _            <- fs.map(workersQueue.put(_)).map(_.start.void).sequence
        pool         = WorkerPoolImpl[F, A, B](workersQueue)
      } yield pool

    private case class WorkerPoolImpl[F[_]: Concurrent, A, B](workersQueue: MVar[F, Worker[F, A, B]])
        extends WorkerPool[F, A, B] {
      override def exec(a: A): F[B] =
        for {
          worker <- workersQueue.take                                            // -- 2,3 --
          result <- worker.run(a).guarantee(workersQueue.put(worker).start.void) // -- 4 --
        } yield result

      override def addWorker(w: Worker[F, A, B]): F[Unit] =
        workersQueue.put(w).start.void

      override def removeAllWorkers: F[Unit] =
        ???
    }
  }

  def getWorker[F[_]: Concurrent](currentID: Ref[F, Int])(implicit timer: Timer[F]): F[Worker[F, Int, Int]] =
    currentID.modify(id => (id + 1) -> mkWorker(id)).flatten

  def testPool[F[_]: Concurrent](nWorkers: Int, currentID: Ref[F, Int])(
    implicit timer: Timer[F]
  ): F[WorkerPool[F, Int, Int]] =
    getWorker(currentID)
      .replicateA(nWorkers)
      .flatMap(WorkerPool.of(_))

  def run(args: List[String]) =
    for {
      nWorkers  <- IO(Try(args(0).toInt).getOrElse(10))
      currentID <- Ref.of[IO, Int](1)
      pool      <- testPool[IO](nWorkers, currentID)
      fiber     <- pool.exec(0 /*ignored*/ ).replicateA(20).start // -- 1 --
      worker    <- getWorker[IO](currentID)
      _         <- pool.addWorker(worker)
      _         <- fiber.join
    } yield ExitCode.Success
}
