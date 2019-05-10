package concurrency

import java.util.concurrent.atomic.AtomicReference

import cats.Parallel
import cats.effect.{Concurrent, IO, Timer}
import cats.effect.concurrent._

import scala.concurrent.duration.FiniteDuration
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.internals.IOContextShift

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Concurrency extends App {

  // Required by ConcurrentIO
  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  trait OwnRef[F[_], A] {
    def get: F[A]
    def set(a: A): F[Unit]
    def update(f: A => A): F[Unit]
//    def modify[B](f: A => (A, B)): F[(A, B)]
  }

  class OwnRefImpl[F[_]: Sync, A](ar: AtomicReference[A]) extends OwnRef[F, A] {
    override def get: F[A] = Sync[F].delay(ar.get)
    override def set(a: A): F[Unit] = Sync[F].delay(ar.set(a))
    override def update(f: A => A): F[Unit] = {
      def loop(): Unit = {
        val oldRef = ar.get
        val updated = f(oldRef)
        if (ar.compareAndSet(oldRef, updated)) () else loop()
      }
      Sync[F].delay(loop())
    }
  }

  object IORef {
    def create[A](v: A): IO[OwnRef[IO, A]] =
      IO.delay(new OwnRefImpl[IO, A](new AtomicReference[A](v)))
  }

  // ------------------------------
  // ------------------------------
  // ------------------------------

  // Debounce in JS
  class Debouncer[F[_]: Concurrent: Timer, A](debounceOver: FiniteDuration,
                                              current: Ref[F, Option[F[Unit]]] /*CancelToken*/
  ) {
    private val delay: F[Unit] = implicitly[Timer[F]].sleep(debounceOver)
    def apply(t: F[A]): F[A] =
      current.access.flatMap {
        case (cancel, trySet) =>
          cancel.sequence_ *> (delay *> t).start.bracket { delayed =>
            trySet(Some(delayed.cancel)).flatMap { successful =>
              if (successful)
                delayed.join
              else
                delayed.cancel *> Concurrent[F].never[A]
            }
          }(_.cancel)
      }
    def cancel: F[Unit] =
      current.modify(c => None -> c).flatMap(_.sequence_)
  }

  // ------------------------------
  // ------------------------------
  // ------------------------------

  // Naive Effectful Unbounded Queue
  class EQueue[F[_]: Concurrent, A](state: Ref[F, Either[Queue[Deferred[F, A]], Queue[A]]]) {
    def enqueue(a: A): F[Unit] =
      state.modify {
        case Right(q) =>
          Right(q :+ a) -> ().pure[F]
        case Left(ws) =>
          val rem = ws.tail
          val next =
            if (rem.isEmpty) Right(Queue.empty[A])
            else Left(rem)
          next -> ws.head.complete(a)
      }.flatten

    // Waits for the result
    def dequeue: F[A] =
      Deferred[F, A].bracket { w =>
        state.modify {
          case Right(q) =>
            if (q.isEmpty) Left(Queue(w)) -> ().pure[F]
            else Right(q.tail) -> w.complete(q.head)
          case Left(ws) =>
            Left(ws :+ w) -> ().pure[F]
        }.flatten *> w.get // semantic blocking
      // !!! w.get: memory leak.
      // If user cancels F[A] before w.get finishes, w will be still in the waiting queue
      // Solution: bracket instead of flatMap { w => ...
      //           and filter w from left queue if necessary
      } { w =>
        state.update(_.leftMap(_.filterNot(_ == w)))
      }
  }

  object EQueue {
    def apply[F[_]: Concurrent, A]: F[EQueue[F, A]] =
      Ref
        .of[F, Either[Queue[Deferred[F, A]], Queue[A]]](Right(Queue.empty))
        .map(new EQueue[F, A](_))
  }

  // ------------------------------
  // ------------------------------
  // ------------------------------

  def sum(state: MVar[IO, Int], list: List[Int]): IO[Int] =
    list match {
      case Nil => state.take
      case x :: xs =>
        state.take.flatMap { current =>
          state.put(current + x).flatMap(_ => sum(state, xs))
        }
    }

  MVar.of[IO, Int](0).flatMap(sum(_, (0 until 100).toList))

  // ----------------------------------------

  // Signaling option, because we need to detect completion
  type Channel[A] = MVar[IO, Option[A]]

  def producer(ch: Channel[Int], list: List[Int]): IO[Unit] =
    list match {
      case Nil =>
        ch.put(None) // we are done!
      case head :: tail =>
        // next please
        (IO(println(s"Put: $head")) *> ch.put(Some(head)), producer(ch, tail)).parMapN((_, _) => ())
    }

  def consumer(ch: Channel[Int], sum: Long): IO[Long] =
    IO(println(s"Read: $sum")) *> ch.take.flatMap {
      case Some(x) =>
        // next please
        consumer(ch, sum + x)
      case None =>
        IO.pure(sum) // we are done!
    }

  val p = for {
    channel <- MVar[IO].empty[Option[Int]]
    count = 1000
    producerTask = producer(channel, (0 until count).toList)
    consumerTask = consumer(channel, 0L)

    fp <- producerTask.start
    fc <- consumerTask.start
    _ <- fp.join
    sum <- fc.join
  } yield sum

//  TODO: it has race condditions
//  p.flatMap(sum => IO(println(sum))).unsafeRunSync()

  // ------------------------------
  // ------------------------------
  // ------------------------------

  class PreciousResource[F[_]](name: String, s: Semaphore[F])(implicit F: Concurrent[F], timer: Timer[F]) {
    def use: F[Unit] =
      for {
        x <- s.available
        _ <- F.delay(println(s"$name >> Availability: $x"))
        _ <- s.acquire
        y <- s.available
        _ <- F.delay(println(s"$name >> Started | Availability: $y"))
        _ <- timer.sleep(3.seconds)
        _ <- s.release
        z <- s.available
        _ <- F.delay(println(s"$name >> Done | Availability: $z"))
      } yield ()
  }

  implicit val timer = IO.timer(ExecutionContext.global)
  implicit val par: Parallel[IO, IO] = Parallel[IO, IO.Par].asInstanceOf[Parallel[IO, IO]]

  val program: IO[Unit] =
    for {
      s <- Semaphore[IO](1)
      r1 = new PreciousResource[IO]("R1", s)
      r2 = new PreciousResource[IO]("R2", s)
      r3 = new PreciousResource[IO]("R3", s)
      _ <- List(r1.use, r2.use, r3.use).parSequence.void
    } yield ()

  program.unsafeRunSync()

  // ------------------------------
  // ------------------------------
  // ------------------------------
}
