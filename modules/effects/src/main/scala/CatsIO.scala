import java.io.{BufferedReader, File, FileReader, RandomAccessFile}
import java.util.concurrent.{Executors, ScheduledExecutorService}
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicBoolean

import cats.FlatMap

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NonFatal
import cats.effect.internals.IOContextShift
import cats.effect._
import cats.effect.concurrent.Deferred
import cats.implicits._
import cats.effect.implicits._

object CatsIO extends App {

  implicit val scheduledExecutorService: ScheduledExecutorService =
    Executors.newScheduledThreadPool(4)
  implicit val cs: ContextShift[IO]     = IOContextShift.global
  implicit val timer: Timer[IO]         = IO.timer(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val global: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  def sleepDIY(d: FiniteDuration)(implicit sc: ScheduledExecutorService): IO[Unit] =
    IO.cancelable { cb =>
      val r = new Runnable { def run() = println("Running"); cb(Right(())) }
      val f = sc.schedule(r, d.length, d.unit)

      IO { println("Canceling runnable"); f.cancel(true) }.map(_ => ())
    }

  // it blocks :shrug:
  val p = for {
    _   <- IO { println("Before") }
    res <- sleepDIY(1.seconds) *> IO.pure(10)
    _   <- IO { println("After") }
  } yield res

  //  Before
  //  After
  //  The result is 10
  //  Running
  //println(s"The result is ${p.unsafeRunSync()}")

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

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

  val fib10000 = for {
    fib <- fib(10000)
    _   <- IO { println(s"Fib of 10000 is $fib") }
  } yield fib

//  fib10000.unsafeRunSync()

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  def unsafeFileToString(file: File, isActive: AtomicBoolean): String = {
    val aFile     = new RandomAccessFile(file, "r")
    val inChannel = aFile.getChannel
    val buffer    = ByteBuffer.allocate(8) // small to make a lot of whiles
    try {
      val sb = new StringBuilder()
      while ({ inChannel.read(buffer) > 0 } && isActive.get()) {
        Thread.sleep(100)
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

  def readFile0(file: File)(implicit ec: ExecutionContext): IO[String] = {
    IO.cancelable[String] { cb =>
      val isActive = new AtomicBoolean(true)
      ec.execute(() => {
        try {
          cb(Right(unsafeFileToString(file, isActive)))
        } catch {
          case NonFatal(e) => cb(Left(e))
        }
      })
      IO { println("Releasing resources in a safe way"); isActive.set(false) }
    }
  }

  type Error = String
  def readFileOrTimeOut(file: File, timeout: FiniteDuration): IO[Either[Error, String]] =
    readFile0(file).timeout(timeout).map { Right.apply }.handleErrorWith { _ =>
      IO.pure(Left("Timeout exception"))
    }

  readFileOrTimeOut(new File("data/lore.txt"), 10.millis)
//    .unsafeRunSync() match {
//    case Right(text) => println(text); ExitCode.Success
//    case Left(e)     => println(e); ExitCode.Error
//  }

  def readFile(file: File, isCancelled: Deferred[IO, Boolean])(implicit ec: ExecutionContext) = {
    IO.cancelable[String] { cb =>
      val isActive = new AtomicBoolean(true)
      ec.execute(() => {
        try {
          cb {
            val content = unsafeFileToString(file, isActive)
            isCancelled.complete(false).handleError(_ => ())
            Right(content)
          }
        } catch {
          case NonFatal(e) =>
            cb {
              isCancelled.complete(true).handleError(_ => ())
              Left(e)
            }
        }
      })
      (for {
        _ <- IO { println("Releasing resources in a safe way"); isActive.set(false) }
        _ <- isCancelled.complete(true).attempt // ignoring the error
      } yield ()).uncancelable
    }
  }

  (for {
    deferred    <- Deferred[IO, Boolean]
    fiber       <- readFile(new File("data/lore.txt"), deferred).start
    _           <- IO.sleep(10.millis) *> fiber.cancel
    isCancelled <- deferred.get
    lore        <- if (isCancelled) IO.pure("Exception") else fiber.join
    _           <- IO(println(lore))
  } yield ()) //.unsafeRunSync()

  /**
    *  1.- Use timeout
    *  2.- Only call .join if you haven't called .cancel. In the simplest case this is an if, in more complicated cases you might need some
    *      coordinator between the interrupter and the joiner
    *  3.- Put the result in a Ref + Deferred thing. Then you need to bracketCase or guaranteeCase the started operation so that it puts None on
    *      that Deferred when it gets cancelled
    */
  // With .join this will prints ~1000 ms
  // Without .join ~20 ms
  def timing[F[_]: Concurrent](implicit timer: Timer[F]): F[Unit] =
    for {
      t0 <- Concurrent[F].delay { System.currentTimeMillis() }
      fiber <- Concurrent[F].start {
                Concurrent[F].uncancelable(timer.sleep(1.seconds))
              }
      _ <- fiber.cancel // can't be cancelled
//      _ <- fiber.join   // will block until fiber is done
      _ <- Concurrent[F].delay { println(System.currentTimeMillis() - t0) }
    } yield ()

//  timing[IO].unsafeRunSync()

  /**
  John A. De Goes
  @jdegoes
  Jan 14 23:25
  @monadplus Instead of fiber.join, call fiber.await. Join says, "the outcome of the fiber will be my outcome", await says, "just let me know what happened".
    */
  /**
    * Author: Fabio Labella
    */
  def await[F[_]: Concurrent, A](fa: F[A]): F[(F[Option[Either[Throwable, A]]], CancelToken[F])] =
    Deferred[F, Option[Either[Throwable, A]]].flatMap { result =>
      val action = {
        fa.attempt.flatMap { r =>
          println("finished"); result.complete(r.some).uncancelable
        }
      }.guaranteeCase {
        case ExitCase.Canceled => println("cancelled"); result.complete(None)
        case _                 => println("not cancelled"); ().pure[F]
      }

      action.start.bracketCase { fiber =>
        println("bracketcase"); (result.get -> fiber.cancel).pure[F]
      } {
        case (fiber, ExitCase.Canceled) => println("cancelling fiber"); fiber.cancel
        case (_, _)                     => ().pure[F]
      }
    }

  // todo: it does pretty weird things..
  val readFileOrNone =
    for {
      (action, cancelToken) <- await(readFile0(new File("data/lore.txt")))
      res                   <- action
//      _                     <- cancelToken
    } yield res

//  println { readFileOrNone.unsafeRunSync() }

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

//  def readLine(in: BufferedReader)(implicit ec: ExecutionContext) =
//    IO.cancelable[String] { cb =>
//      ec.execute(() => cb(
//        try Right(in.readLine())
//        catch { case NonFatal(e) => Left(e) }))
//
//      // Cancellation logic is not thread-safe!
//      IO(in.close())
//    }

  def readLine(in: BufferedReader)(implicit ec: ExecutionContext) =
    IO.cancelable[String] { cb =>
      val isActive = new AtomicBoolean(true)
      ec.execute { () =>
        if (isActive.getAndSet(false)) {
          try cb(Right(in.readLine()))
          catch { case NonFatal(e) => cb(Left(e)) }
        }
      // Note there's no else; if cancellation was executed
      // then we don't call the callback; task becoming
      // non-terminating ;-)
      }
      // Cancellation logic
      IO {
        // Thread-safe gate
        if (isActive.getAndSet(false))
          in.close()
      }
    }

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  val launchMissiles = IO.raiseError(new Exception("Boom!"))
  val runToBunker    = IO(println("To the bunker!"))

  def launchAndRun(implicit cs: ContextShift[IO]): IO[Unit] =
    for {
      fiber <- (IO.sleep(1.seconds) *> runToBunker).start
      _ <- launchMissiles.handleErrorWith { _ =>
            fiber.cancel *> IO(println("Something went wrong, cancelling missiles launching"))
          }
//      _ <- fiber.join // blocks
    } yield ()

//  launchAndRun.unsafeRunSync()

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  def fib2(n: Int, a: Long, b: Long): IO[Long] =
    IO.suspend {
      if (n <= 0) IO.pure(a)
      else {
        val next = IO(println(s"Iteration: $n")) *> fib2(n - 1, b, a + b)
        if (n % 100 == 0)
          IO.cancelBoundary *> next
        else
          next
      }
    }

  val cancellableFib = for {
    fiber <- fib2(1000, 0, 1).start
    _     <- IO.sleep(1.millis) *> fiber.cancel
  } yield ()

//  cancellableFib.unsafeRunSync() // Computes 100 and finishes

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  def race[F[_]: Concurrent, A, B](lh: F[A], rh: F[B]): F[Either[A, B]] = {
    FlatMap[F].flatMap(Concurrent[F].racePair(lh, rh)) {
      case Left((a, fiberB))  => fiberB.cancel.map(_ => Left(a))
      case Right((fiberA, b)) => fiberA.cancel.map(_ => Right(b))
    }
  }

  def timeOut[F[_]: Concurrent, A](
      fa: F[A]
  )(duration: FiniteDuration)(implicit timer: Timer[F]): F[Option[A]] =
    race(fa, timer.sleep(duration)).flatMap {
      case Left(a) => a.some.pure[F]
      case _       => none[A].pure[F]
    }

  val readFileOrTimeOut =
    for {
      res <- timeOut(readFile0(new File("data/lore.txt")))(10.millis)
    } yield res

//  println(readFileOrTimeOut.unsafeRunSync())

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  def readFirstLine(file: File): IO[String] =
    IO(new BufferedReader(new FileReader(file))).bracket { in =>
      IO(in.readLine())
    } { in =>
      IO(in.close())
    }

  // todo: use and release can be executed at the same time so this wont work !
  def readFileNonBlocking(file: File): IO[String] = {
    val acquire = IO.shift *> IO(new BufferedReader(new FileReader(file)))
    acquire.bracket { in =>
      IO {
        val content      = new StringBuilder()
        var line: String = null
        do {
          line = in.readLine()
          if (line != null) content.append(line)
        } while (line != null)
        content.toString()
      }
    } { in =>
      // Releasing the reader (the finally block)
      // This is problematic if the resulting `IO` can get
      // canceled, because it can lead to data corruption
      IO(in.close())
    }
  }

  def readFileNonBlocking2(file: File): IO[String] = {

    val isActive = new AtomicBoolean(true)
    val acquire  = IO.shift *> IO(new BufferedReader(new FileReader(file)))

    IO.suspend {
      acquire.bracket { in =>
        IO {
          val content      = new StringBuilder()
          var line: String = null
          do {
            line = in.synchronized {
              if (isActive.get())
                in.readLine()
              else
                null
            }
            if (line != null) content.append(line)
          } while (line != null)
          content.toString()
        }
      } { in =>
        IO(isActive.set(false)) *> IO { in.synchronized { in.close() } }
      }
    }
  }

}
