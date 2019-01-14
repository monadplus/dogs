package exercises

import java.io.{File, RandomAccessFile}
import java.util.concurrent.{Executors, ScheduledExecutorService}
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicBoolean

import cats.effect.internals.IOContextShift
import cats.effect.{ContextShift, IO}
import cats.implicits._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import concurrent.duration._
import scala.util.control.NonFatal

object Future2IOAsync {
  // IO.fromFuture
  def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    fa.value match {
      case Some(result) =>
        println("Completed")
        result match {
          case Success(value) => IO.pure(value)
          case Failure(e)     => IO.raiseError(e)
        }
      case None =>
        println("Uncompleted")
        IO.async { cb =>
          fa.onComplete {
            case Success(value) => cb(Right(value))
            case Failure(e)     => cb(Left(e))
          }
        }
    }
}

object Cancellable extends App {
  implicit val scheduledExecutorService: ScheduledExecutorService =
    Executors.newScheduledThreadPool(4)

  def sleep(d: FiniteDuration)(implicit sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run() = println("Running"); cb(Right(())) } // This will exit before calling after.. cb(Right(System.exit(0))) }
      val f = sc.schedule(r, d.length, d.unit)

      // Returning the cancellation token needed to cancel
      // the scheduling and release resources early
      IO { println("Canceling runnable"); f.cancel(true) }.map(_ => ())
    }

  val p = for {
    _   <- IO { println("Before") }
    res <- sleep(1.seconds) *> IO.pure(10)
    _   <- IO { println("After") }
  } yield res

  //  Before
  //  After
  //  The result is 10
  //  Running
  println(s"The result is ${p.unsafeRunSync()}")

  // Stack safe because of IO.suspend (flatMap is also stack safe..)
  def fib(n: Int)(implicit cs: ContextShift[IO]): IO[Long] = {
    def go(n: Int, a: Long, b: Long): IO[Long] =
      IO.suspend {
        if (n == 0) IO.pure(a)
        else {
          val next = go(n - 1, b, a + b)
          if (n % 10 == 0)
            // Changes thread or call stack (does not prevent stackoverflow)
            cs.shift *> IO { println(Thread.currentThread().getName) } *> next
          else
            next
        }
      }

    def go2(n: Int, a: Long, b: Long): IO[Long] =
      IO.suspend {
        if (n > 0)
          go2(n - 1, b, a + b)
        else
          IO.pure(a)
      }

    go(n, 0, 1)
  }

  // Same as IOApp
  implicit val cs: ContextShift[IO] =
    IOContextShift.global

  (for {
    fib <- fib(10000)
    _   <- IO { println(s"Fib of 10000 is $fib") }
  } yield fib).unsafeRunSync()

  def unsafeFileToString(file: File, isActive: AtomicBoolean): String = {
    val aFile     = new RandomAccessFile(file, "r")
    val inChannel = aFile.getChannel
    val buffer    = ByteBuffer.allocate(1024)
    try {
      val sb = new StringBuilder()
      while ({ inChannel.read(buffer) > 0 } && isActive.get()) {
        buffer.flip
        while (buffer.hasRemaining) {
          sb.append(buffer.get().toChar)
        }
        buffer.clear
      }
      sb.toString
    } finally {
      inChannel.close()
      aFile.close()
    }
  }

  def readFile(file: File)(implicit ec: ExecutionContext) = {
    IO.cancelable[String] { cb =>
      val isActive = new AtomicBoolean(true)

      ec.execute(() => {
        try {
          cb(Right(unsafeFileToString(file, isActive)))
        } catch {
          case NonFatal(e) =>
            cb(Left(e))
        }
      })
      IO { isActive.set(false) }
    }
  }

  val path = "data/lore.txt"
  println(
    s"Reading file $path ...\n${readFile(new File(path))(scala.concurrent.ExecutionContext.Implicits.global).unsafeRunSync()}"
  )
}
