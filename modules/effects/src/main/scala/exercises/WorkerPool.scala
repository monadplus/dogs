package exercises

import cats.effect.{ExitCode, IOApp}

//   Requirements:
//   1. Processing jobs must run in parallel
//   2. Submitting a processing request must wait if all workers are busy.
//   3. Submission should do load balancing: wait for the first worker to finish, not for a certain one.
//   4. Worker should become available whenever a job is completed successfully, with an exception or cancelled.

object WorkerPoolExercise extends IOApp {
  import cats._, cats.data._, cats.implicits._
  import cats.effect._, cats.effect.implicits._, cats.effect.concurrent._

  import scala.util.Random
  import scala.concurrent.duration._
  import scala.util._

  type Id                 = Int
  type Worker[F[_], A, B] = A => F[B]

  def mkWorker[F[_]: Concurrent](implicit timer: Timer[F]): F[Worker[F, Int, Int]] =
    Ref[F].of(0).map { counter =>
      def simulateWork: F[Unit] =
        Sync[F].delay(50 + Random.nextInt(450)).map(_.millis).flatMap(timer.sleep)

      def report: F[Unit] =
        counter.get.flatMap(i => Sync[F].delay(println(s"Total processed $i")))

      val f: Int => F[Int] =
        x =>
          simulateWork >>
            counter.update(_ + 1) >>
            report >>
            Applicative[F].pure(x + 1)

      f
    }

  trait WorkerPool[F[_], A, B] {
    def exec(a: A): F[B]
    def removeAllWorkers: F[Unit]
    def addWorker(w: Worker[F, A, B]): F[Unit]
  }

  object WorkerPool {
    def of[F[_]: Concurrent, A, B](ws: List[Worker[F, A, B]]): F[WorkerPool[F, A, B]] =
      for {
        ids          <- Ref.of[F, Int](0)
        workersQueue <- MVar[F].empty[(Id, Worker[F, A, B])]
        active       <- Ref.of[F, Set[Id]](Set.empty)
        _ <- ws.traverse { w =>
              ids.modify(id => (id + 1) -> active.update(_ + id) *> workersQueue.put(id -> w).start.void).flatten
            }
      } yield WorkerPoolImpl(workersQueue, active, ids)

    private case class WorkerPoolImpl[F[_]: Concurrent, A, B](
      workersQueue: MVar[F, (Id, Worker[F, A, B])],
      active: Ref[F, Set[Id]],
      ids: Ref[F, Int]
    ) extends WorkerPool[F, A, B] {

      private def re_enqueue(id: Id, w: Worker[F, A, B]): F[Unit] =
        for {
          activeIds <- active.get
          _         <- workersQueue.put(id -> w).start.whenA(activeIds contains id)
        } yield ()

      override def exec(a: A): F[B] =
        for {
          (id, worker) <- workersQueue.take
          result       <- worker(a).guarantee(re_enqueue(id, worker))
        } yield result

      override def addWorker(w: Worker[F, A, B]): F[Unit] =
        for {
          id <- ids.modify(id => (id + 1) -> id)
          _  <- workersQueue.put(id -> w).start.void
          _  <- active.update(_ + id)
        } yield ()

      override def removeAllWorkers: F[Unit] =
        active.set(Set.empty)
    }
  }

  def testPool[F[_]: Concurrent](nWorkers: Int)(
    implicit timer: Timer[F]
  ): F[WorkerPool[F, Int, Int]] =
    mkWorker
      .replicateA(nWorkers)
      .flatMap(WorkerPool.of(_))

  def run(args: List[String]) =
    for {
      nWorkers <- IO(Try(args(0).toInt).getOrElse(10))
      pool     <- testPool[IO](nWorkers)
      _        <- pool.removeAllWorkers
      fiber    <- pool.exec(0 /*ignored*/ ).replicateA(20).start
      w        <- mkWorker[IO]
      _        <- pool.addWorker(w)
      _        <- fiber.join
    } yield ExitCode.Success
}
