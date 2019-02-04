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

  type Id = Int
  final case class Worker[F[_], A, B](id: Int, run: A => F[B])

  def mkWorker[F[_]: Concurrent](implicit timer: Timer[F]): F[Id => Worker[F, Int, Int]] =
    Ref[F].of(0).map { counter => (id: Id) =>
      {
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

        Worker(id, f)
      }
    }

  trait WorkerPool[F[_], A, B] {
    def exec(a: A): F[B]
    def addWorker(implicit timer: Timer[F]): F[Unit]
    def removeAllWorkers: F[Unit]
  }

  object WorkerPool {
    def of[F[_]: Concurrent](fs: List[Id => Worker[F, Int, Int]]): F[WorkerPool[F, Int, Int]] =
      for {
        ids          <- Ref.of[F, Int](0)
        workersQueue <- MVar[F].empty[Worker[F, Int, Int]]
        workers      <- fs.traverse(f => ids.modify(id => (id + 1) -> f(id)))
        _            <- workers.traverse(workersQueue.put(_).start.void)
        active       <- Ref.of[F, Set[Id]](workers.map(_.id).toSet)
      } yield WorkerPoolImpl[F](workersQueue, active, ids)

    private case class WorkerPoolImpl[F[_]: Concurrent](
      workersQueue: MVar[F, Worker[F, Int, Int]],
      active: Ref[F, Set[Id]],
      ids: Ref[F, Int]
    ) extends WorkerPool[F, Int, Int] {

      private def re_enqueue(w: Worker[F, Int, Int]): F[Unit] =
        for {
          ids <- active.get
          _   <- workersQueue.put(w).start.whenA(ids contains w.id)
        } yield ()

      override def exec(a: Int): F[Int] =
        for {
          worker <- workersQueue.take
          result <- worker.run(a).guarantee(re_enqueue(worker))
        } yield result

      override def addWorker(implicit timer: Timer[F]): F[Unit] =
        for {
          w <- ids.modify(id => (id + 1) -> mkWorker.map(_(id))).flatten
          _ <- workersQueue.put(w).start.void
          _ <- active.update(_ + w.id)
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
      _        <- pool.addWorker
      _        <- pool.addWorker
      _        <- fiber.join
    } yield ExitCode.Success
}
