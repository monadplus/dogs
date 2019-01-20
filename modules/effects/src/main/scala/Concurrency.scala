import java.util.concurrent.atomic.AtomicReference

import cats.effect.{Concurrent, IO, Timer}
import cats.effect.concurrent._

import scala.concurrent.duration.FiniteDuration
import cats.implicits._
import cats.effect.implicits._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Concurrency {
  trait OwnRef[F[_], A] {
    def get: F[A]
    def set(a: A): F[Unit]
    def update(f: A => A): F[Unit]
//    def modify[B](f: A => (A, B)): F[(A, B)]
  }

  class IORef[A](ar: AtomicReference[A]) extends OwnRef[IO, A] {
    override def get: IO[A] = IO(ar.get())
    override def set(a: A): IO[Unit] = IO(ar.set(a))
    override def update(f: A => A): IO[Unit] = {
      @tailrec
      def loop(): Unit = {
        val oldRef = ar.get()
        val updated = f(oldRef)
        if (ar.compareAndSet(oldRef, updated)) () else loop()
      }
      IO(loop())
    }
  }

  object IORef {
    def create[A](v: A): IO[OwnRef[IO, A]] =
      IO.delay(new IORef(new AtomicReference[A](v)))
  }

  // ------------------------------
  // ------------------------------
  // ------------------------------

  // Debounce in JS
  class Debouncer[F[_]: Concurrent: Timer, A](debounceOver: FiniteDuration,
                                              current: Ref[F, Option[F[Unit]]] /*CanceToken*/
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
  // Left:  no elements, people waiting for elements
  // Right: bunch of elements already in the queue waiting for someone to pick them up
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

}
