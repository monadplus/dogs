package exercises

import cats.implicits._
import cats.effect._
import cats.effect.concurrent._

import fs2._
import fs2.concurrent.Queue

import scala.util.Random
import scala.concurrent.duration._

// - Processing jobs must run in parallel
// - Submitting a processing request must wait if all workers are busy.
// - Submission should do load balancing: wait for the first worker to finish, not for a certain one.
// - Worker should become available whenever a job is completed successfully, with an exception or cancelled.
// - Add methods to WorkerPool interface for adding workers on the fly and removing all workers.
//   If all workers are removed, submitted jobs must wait until one is added.

object StreamWorkerPool extends IOApp {

  type Worker[A, B] = A => IO[B]

  final class Id

  case class IdWorker[A, B](id: Id, w: Worker[A, B]) extends (A => IO[B]) {
    override def apply(v1: A): IO[B] = w(v1)
  }

  def mkWorker(id: Int)(implicit timer: Timer[IO]): IO[Worker[Int, Int]] =
    Ref[IO].of(0).map { counter =>
      def simulateWork: IO[Unit] =
        IO(50 + Random.nextInt(450)).map(_.millis).flatMap(IO.sleep)

      def report: IO[Unit] =
        counter.get.flatMap(i => IO(println(s"Total processed by $id: $i")))

      def failureOrUnit: IO[Unit] =
        IO(Random.nextInt(10))
          .map(_ < 5)
          .flatMap((IO(println(s"$id encountered an error.")) >> IO.raiseError[Unit](
            new Exception("boom"))).whenA(_))

      x =>
        simulateWork >>
          counter.update(_ + 1) >>
          report >>
          failureOrUnit >>
          IO.pure(x + 1)
    }

  trait WorkerPool[A, B] {
    def exec(a: A): IO[B]
    def addWorker(w: Worker[A, B]): IO[Unit]
    def removeAllWorkers: IO[Unit]
  }

  object WorkerPool {
    def of[A, B](fs: List[Worker[A, B]]): IO[WorkerPool[A, B]] =
      Queue.unbounded[IO, IdWorker[A, B]].flatMap { queue =>
        Ref.of[IO, Set[Id]](Set.empty).flatMap { ids =>
          def reQueue(w: IdWorker[A, B]): IO[Unit] =
            ids.get.flatMap { ids =>
              queue.enqueue1(w).whenA(ids.contains(w.id))
            }

          // Maybe races.
          def enqueue(w: Worker[A, B]): IO[Unit] =
            ids
              .modify { s =>
                val id = new Id
                (s + id) -> id
              }
              .flatMap { id =>
                val worker = IdWorker(id, w)
                queue.enqueue1(worker)
              }

          Stream
            .emits(fs)
            .evalMap(enqueue)
            .compile
            .drain
            .uncancelable
            .map { _ =>
              new WorkerPool[A, B] {

                def exec(a: A): IO[B] =
                  queue.dequeue1.flatMap { w =>
                    w(a).guarantee(reQueue(w))
                  }

                def addWorker(w: Worker[A, B]): IO[Unit] =
                  enqueue(w)

                def removeAllWorkers: IO[Unit] =
                  ids.set(Set.empty).flatMap { _ =>
                    queue.dequeue.compile.drain
                  }
              }
            }
        }
      }
  }

  val testPool: IO[WorkerPool[Int, Int]] =
    List
      .range(0, 10)
      .traverse(mkWorker)
      .flatMap(WorkerPool.of)

  def testRemoveAll() =
    for {
      pool <- WorkerPool.of[Unit, Unit](List.fill(10)(_ => IO.sleep(2.seconds) >> IO(println("after wait"))))
      _    <- pool.exec(()).start
      _    <- pool.removeAllWorkers
      _    <- pool.exec(()) >> IO(println("after exec"))
    } yield ExitCode.Error

  def testConcurrency() =
    testPool
      .flatMap { pool =>
        val remove = Stream.awakeEvery[IO](2.seconds).evalMap(_ => pool.removeAllWorkers)
        val add = Stream.awakeEvery[IO](100.millis).evalMap(_ => mkWorker(0).flatMap(w => pool.addWorker(w)))
        val both = remove.concurrently(add)
        val run =
          Stream
            .range[IO](0, 100)
            .mapAsync(4) { v =>
              // The main problem with this is waiting for an available worker is also considered "canceled"
              // IO.race(pool.exec(v).attempt, IO.sleep(400.millis) >> IO(println("canceled")))
              pool.exec(v).attempt
            }

        run.concurrently(both).compile.drain
      }.as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] =
    testConcurrency()

}
