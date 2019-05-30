package exercises

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.concurrent.{Deferred, Ref}

import scala.collection.immutable.Queue

abstract class Semaphore[F[_]] {

  /**
    * Returns the number of permits currently available. Always non-negative.
    *
    * May be out of date the instant after it is retrieved.
    * Use `[[tryAcquire]]` or `[[tryAcquireN]]` if you wish to attempt an
    * acquire, returning immediately if the current count is not high enough
    * to satisfy the request.
    */
  def available: F[Long]

  /**
    * Obtains a snapshot of the current count. May be negative.
    *
    * Like [[available]] when permits are available but returns the number of permits
    * callers are waiting for when there are no permits available.
    */
  def count: F[Long]

  /**
    * Acquires `n` permits.
    *
    * The returned effect semantically blocks until all requested permits are
    * available. Note that acquires are statisfied in strict FIFO order, so given
    * `s: Semaphore[F]` with 2 permits available, an `acquireN(3)` will
    * always be satisfied before a later call to `acquireN(1)`.
    *
    * @param n number of permits to acquire - must be >= 0
    */
  def acquireN(n: Long): F[Unit]

  /** Acquires a single permit. Alias for `[[acquireN]](1)`. */
  def acquire: F[Unit] = acquireN(1)

  /**
    * Acquires `n` permits now and returns `true`, or returns `false` immediately. Error if `n < 0`.
    *
    * @param n number of permits to acquire - must be >= 0
    */
  def tryAcquireN(n: Long): F[Boolean]

  /** Alias for `[[tryAcquireN]](1)`. */
  def tryAcquire: F[Boolean] = tryAcquireN(1)

  /**
    * Releases `n` permits, potentially unblocking up to `n` outstanding acquires.
    *
    * @param n number of permits to release - must be >= 0
    */
  def releaseN(n: Long): F[Unit]

  /** Releases a single permit. Alias for `[[releaseN]](1)`. */
  def release: F[Unit] = releaseN(1)

  /**
    * Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit.
    */
  def withPermit[A](t: F[A]): F[A]
}

object Semaphore {

  def apply[F[_]](n: Long)(implicit F: Concurrent[F]): F[Semaphore[F]] = {
    assertNonNegative[F](n) *>
      Ref.of[F, State[F]](Right(n)).map(stateRef => new ConcurrentSemaphore(stateRef))
  }

  private final class ConcurrentSemaphore[F[_]](state: Ref[F, State[F]])(implicit F: Concurrent[F]) extends AbstractSemaphore(state) {
    protected def mkGate: F[Deferred[F, Unit]] = Deferred[F, Unit]
  }

  private def assertNonNegative[F[_]](n: Long)(implicit F: ApplicativeError[F, Throwable]): F[Unit] =
    if (n < 0) F.raiseError(new IllegalArgumentException(s"n must be nonnegative, was: $n")) else F.unit

  private type State[F[_]] = Either[Queue[(Long, Deferred[F, Unit])], Long]

  private abstract class AbstractSemaphore[F[_]](state: Ref[F, State[F]])(implicit F: Async[F]) extends Semaphore[F] {

    // Concurrent or Sync
    protected def mkGate: F[Deferred[F, Unit]]

    /**
      * Returns the number of permits currently available. Always non-negative.
      *
      * May be out of date the instant after it is retrieved.
      * Use `[[tryAcquire]]` or `[[tryAcquireN]]` if you wish to attempt an
      * acquire, returning immediately if the current count is not high enough
      * to satisfy the request.
      */
    override def available: F[Long] =
      state.get.map {
        case Left(_) => 0
        case Right(n) => n
      }

    /**
      * Obtains a snapshot of the current count. May be negative.
      *
      * Like [[available]] when permits are available but returns the number of permits
      * callers are waiting for when there are no permits available.
      */
    override def count: F[Long] =
      state.get.map {
        case Left(q) => - q.map(_._1).sum
        case Right(n) => n
      }

    /**
      * Acquires `n` permits.
      *
      * The returned effect semantically blocks until all requested permits are
      * available. Note that acquires are statisfied in strict FIFO order, so given
      * `s: Semaphore[F]` with 2 permits available, an `acquireN(3)` will
      * always be satisfied before a later call to `acquireN(1)`.
      *
      * @param n number of permits to acquire - must be >= 0
      */
    override def acquireN(n: Long): F[Unit] =
      F.bracketCase(acquireNInternal(n)){case (a, _) => a} {
        // The only possible error is `assertNonNegative`
        case ((_, r), ExitCase.Canceled) => r
        case _ => F.unit
      }

    //                                        acquire   release
    private def acquireNInternal(n: Long): F[(F[Unit], F[Unit])] =
      assertNonNegative[F](n) >> {
        if (n == 0) F.pure(F.unit -> F.unit)
        else {
          mkGate.flatMap { gate =>
            state.modify { old =>
              val u = old match {
                case Left(q) => Left(q :+ (n -> gate))
                case Right(m) =>
                  if (n <= m) Right(m - n)
                  else Left(Queue((n - m) -> gate))
              }
              (u, u)
            }.map {
              case Left(waiting) =>
                val cleanup: F[Unit] = state.modify {
                  case Left(waiting) =>
                    // The current gate.get was cancelled, no need to cancel it.
                    // But we must remove it and release the permits afterwards
                    waiting.find(_._2 eq gate).map(_._1) match {
                      case None =>  Left(waiting) -> releaseN(n)
                      case Some(m) => Left(waiting.filterNot(_._2 eq gate)) -> releaseN(n - m)
                    }
                  case Right(m) => Right(m + n) -> F.unit
                }.flatten
                // I don't get why they do this instead of a simple gate.get
                val entry = waiting.lastOption.getOrElse(sys.error("Empty waiting queue instead of 0 permits"))
                entry._2.get -> cleanup
              case Right(_) =>
                F.unit -> releaseN(n)
            }
          }
        }
      }

    /**
      * Acquires `n` permits now and returns `true`, or returns `false` immediately. Error if `n < 0`.
      *
      * @param n number of permits to acquire - must be >= 0
      */
    override def tryAcquireN(n: Long): F[Boolean] =
      assertNonNegative[F](n) *> {
        if (n == 0) F.pure(true)
        else
          state.modify {
            case s@Left(_) =>
              s -> false
            case s@Right(m) =>
              if (n <= m) Right(m - n) -> true
              else s -> false
          }
      }

    /**
      * Releases `n` permits, potentially unblocking up to `n` outstanding acquires.
      *
      * @param n number of permits to release - must be >= 0
      */
    override def releaseN(n: Long): F[Unit] =
      assertNonNegative[F](n) *> {
        if (n == 0) F.unit
        else
          state.modify { old =>
            // Updating mutable state
            val updated: State[F] = old match {
              case Left(q) =>
                var m = n
                var q2 = q
                while(q2.nonEmpty && m > 0) {
                  val (k, gate) = q2.head
                  if (k > m) {
                    q2 = (k - m -> gate) +: q2.tail
                    m = 0
                  } else {
                    q2 = q2.tail
                    m -= k
                  }
                }
                if (q2.isEmpty) Right(m)
                else Left(q2)

              case Right(m) =>
                Right(n + m)
            }
            (updated, (old, updated))
          }.flatMap {
            // Releasing
            case (old, updated) =>
              old match {
                case Left(o) =>
                  val size = updated match {
                    case Left(u) => u.length
                    case Right(_) => 0
                  }
                  val sizeDiff = o.length - size
                  o.take(sizeDiff).foldRight(F.unit) { case ((_, gate), acc) =>
                    acc >> gate.complete(())
                  }
                case Right(_) =>
                  F.unit
              }
          }
      }

    /**
      * Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit.
      */
    override def withPermit[A](t: F[A]): F[A] =
      F.bracket(acquireNInternal(1)) {case (a, _) => a >> t} { case (_, r) => r }
  }


}
