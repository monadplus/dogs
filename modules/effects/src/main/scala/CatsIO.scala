import java.io.{BufferedReader, File, FileReader, RandomAccessFile}
import java.util.concurrent.{Executors, ScheduledExecutorService}
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicBoolean

import cats.{ApplicativeError, FlatMap}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NonFatal
import cats.effect.internals.IOContextShift
import cats.effect._
import cats.effect.concurrent.{Deferred, MVar, Ref}
import cats.implicits._
import cats.effect.implicits._
import cats.effect.ExitCase.{Canceled, Completed, Error}
import scala.util._ // Try 

object CatsIO extends App {

  implicit val scheduledExecutorService: ScheduledExecutorService =
    Executors.newScheduledThreadPool(4)
  implicit val cs: ContextShift[IO]     = IOContextShift.global
  implicit val global: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  implicit val timer: Timer[IO]         = IO.timer(global)

  type Callback[-A] = Either[Throwable, A] => Unit

  // def asyncF[A](k: (Either[Throwable, A] => Unit) => IO[Unit]): IO[A] =
  class PurePromise[F[_], A](ref: Ref[F, Either[List[Callback[A]], A]])(implicit F: Async[F]) {
    def get: F[A] = F.asyncF { cb =>
      ref.modify {
        case current @ Right(value) =>
          (current, F.delay(cb(Right(value))))
        case Left(waiting) =>
          (Left(cb :: waiting), F.unit)
      }
    }

    def complete(value: A): F[Unit] =
      F.flatten(
        ref.modify {
          case current @ Right(_) =>
            (current, F.unit)
          case Left(waiting) =>
            (Right(value), F.delay(waiting.foreach(cb => cb(Right(value)))))
        }
      )
  }

  // IO is trampolined
  def loop[F[_]: Async](n: Int): F[Int] =
    for {
      n <- Async[F].async[Int] { cb =>
            if (n == 100)
              cb(Left(new RuntimeException("boom")))
            else
              cb(Right(n))
          }
      res <- if (n > 0) loop(n - 1) else 0.pure[F]
    } yield res

//  loop[IO](10000).unsafeRunSync()

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

  /** When to use IO.shift:
   *
   * *          - shifting blocking actions off of the main compute pool,
   * *          - defensively re-shifting asynchronous continuations back
   * *            to the main compute pool
   * *          - yielding control to some underlying pool for fairness
   * *            reasons, and
   * *          - preventing an overflow of the call stack in the case of
   * *            improperly constructed `async` actions
   *
   * */
  // Stack safe because of IO.suspend (flatMap is also stack safe..)
  def fib(n: Int)(implicit cs: ContextShift[IO]): IO[Long] = {
    def go(n: Int, a: Long, b: Long): IO[Long] =
      IO.suspend {
        if (n == 0) IO.pure(a)
        else {
          val next = go(n - 1, b, a + b)
          if (n % 10 == 0)
            cs.shift *> IO { println(Thread.currentThread().getName) } *> next
          else
            next
        }
      }

//    def go(n: Int, a: Long, b: Long): IO[Long] =
//      IO.suspend {
//        if (n > 0)
//          go(n - 1, b, a + b)
//        else
//          IO.pure(a)
//      }

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
    println("unsafeFileToString")
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

  def readFile0(file: File)(implicit ec: ExecutionContext): IO[String] =
    // TODO: this won't work as IO is uncancelable 
    // IO(new AtomicBoolean(true)).bracketCase { isActive =>
    //   IO.async[String] { cb =>
    //     ec.execute(new Runnable() {
    //       override def run(): Unit =
    //         cb(Try(unsafeFileToString(file, isActive)).toEither)
    //     })
    //   }
    // } { (isActive, exit) => 
    //   exit match {
    //     case Completed => IO("completed") *>IO.unit
    //     case Error(_) | Canceled => IO("canceled") *> IO(isActive.set(false))
    //   }
    // }.start
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

  def readFileOrTimeOut(file: File, timeout: FiniteDuration): IO[Either[Throwable, String]] =
    readFile0(file).timeout(timeout).attempt

  readFileOrTimeOut(new File("data/lore.txt"), 100.millis).unsafeRunSync() match {
    case Right(text) => println(text); ExitCode.Success
    case Left(e)     => println(e); ExitCode.Error
  }

  def readFile(file: File, isCancelled: Deferred[IO, Boolean])(implicit ec: ExecutionContext) =
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
          result.complete(r.some).uncancelable
        }
      }.guaranteeCase {
        case ExitCase.Canceled => Concurrent[F].delay(println("Canceled")) *> result.complete(None)
        case _                 => ().pure[F]
      }

      action.start.bracketCase { fiber =>
        (result.get -> fiber.cancel).pure[F]
      } {
        case (fiber, ExitCase.Canceled) => fiber.cancel
        case (_, _)                     => ().pure[F]
      }
    }

  val readFileOrNone =
    for {
      (result, cancelToken) <- await(readFile0(new File("data/lore.txt")))
      _                     <- IO.sleep(2.seconds) <* cancelToken
      text                  <- result // this will block
    } yield text

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
        } else {
          cb(Left(new RuntimeException("Cancelled")))
        }
      // [old] Note there's no else; if cancellation was executed
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

  def race[F[_]: Concurrent, A, B](lh: F[A], rh: F[B]): F[Either[A, B]] =
    FlatMap[F].flatMap(Concurrent[F].racePair(lh, rh)) {
      case Left((a, fiberB))  => fiberB.cancel.map(_ => Left(a))
      case Right((fiberA, b)) => fiberA.cancel.map(_ => Right(b))
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

  def parTupled[F[_]: Concurrent, A, B](fa: F[A])(fb: F[B]): F[(A, B)] =
    fa.start.bracket { fiberA =>
      fb.start.bracket { fiberB =>
        (fiberA.join, fiberB.join).tupled // map2
      }(_.cancel)
    }(_.cancel)

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

    val printThread = IO(println(s"Thread: ${Thread.currentThread().getName}"))
    val isActive    = new AtomicBoolean(true)
    val acquire     = IO.shift *> IO(new BufferedReader(new FileReader(file)))

    printThread *> IO.suspend {
      acquire.bracket { in =>
        println(s"Shifted: ${Thread.currentThread().getName}")
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
            Thread.sleep(100)
            if (line != null) content.append(line)
          } while (line != null)
          println("do while finished")
          content.toString()
        }
      } { in =>
        IO(println("cancelled or finished!")) *> IO(isActive.set(false)) *> IO {
          in.synchronized { in.close() }
        }
      }
    }
  }

  val rfnb = for {
    _     <- IO(println(s"Main thread: ${Thread.currentThread().getName}"))
    fiber <- readFileNonBlocking2(new File("data/lore.txt")).start
    _     <- IO.sleep(500.millis) *> IO(println("cancelling")) *> fiber.cancel
  } yield ()

//  rfnb.unsafeRunSync()
//  println("finish")

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  // Error Handling

  def exponentialBackOff[M[_], A](ma: M[A])(maxRetries: Int)(implicit
                                                             m: ApplicativeError[M, Throwable],
                                                             timer: Timer[M]): M[A] = {
    def loop(retries: Int)(delay: FiniteDuration): M[A] =
      ma.handleErrorWith { error =>
        if (retries > 0)
          timer.sleep(delay) *> loop(retries - 1)(delay * 2)
        else
          m.raiseError(new RuntimeException(s"[$maxRetries retries] $error"))
      }

    loop(maxRetries)(100.millis)
  }

  val p2 =
    for {
      res <- exponentialBackOff(IO.raiseError(new RuntimeException("boom!")))(maxRetries = 3).attempt
      _ <- res match {
            case Left(e)  => IO(println(e.getMessage))
            case Right(_) => IO.unit
          }
    } yield ()

//  p2.unsafeRunSync()

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  // Parallel type class

  def runAfter[A](fa: IO[A], duration: FiniteDuration)(implicit timer: Timer[IO]): IO[A] =
    (IO.sleep(duration) *> IO.suspend(fa)).guaranteeCase {
      case ExitCase.Canceled => IO(println("Canceled"))
      case _                 => IO.unit
    }

  val ioa  = IO(println(s"Running on thread: ${Thread.currentThread().getName}")) *> IO.pure(1)
  val boom = IO.raiseError[Int](new RuntimeException("boom!"))

  (runAfter(ioa, 100.millis), runAfter(ioa, 150.millis), runAfter(ioa, 500.millis))
    .parMapN { case (a, b, c) => a + b + c }
//    .unsafeRunSync()

  // Whole computation will fail but it will release resources
  (runAfter(ioa, 1.seconds), runAfter(boom, 150.millis))
    .parMapN { case (a, b) => a + b }
//    .unsafeRunSync()

//  List.fill(10)(runAfter(ioa, 100.millis)).parSequence.unsafeRunSync()
  List(100, 200, 300)
    .parTraverse { i =>
      runAfter(ioa, i.millis)
    }
//    .unsafeRunSync()

  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------
  // --------------------------------------------------

  // Asynchronous boundary - thread scheduling - fairness

  val ecOne = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)
  val ecTwo = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)

  val csOne = IO.contextShift(ecOne)
  val csTwo = IO.contextShift(ecTwo)

  def infiniteIO(id: Int)(implicit cs: ContextShift[IO]): IO[Fiber[IO, Unit]] = {
    def repeat: IO[Unit] = IO(println(id)).flatMap(_ => repeat)
    repeat.start
  }

  val p1 =
    for {
      _ <- infiniteIO(1)(csOne)
      _ <- infiniteIO(11)(csOne) // won't execute, 1 has preempted the only thread
    } yield ()

//  p1.unsafeRunSync()

  val p3 =
    for {
      _ <- infiniteIO(1)(csOne)
      _ <- infiniteIO(11)(csOne) // won't print
      _ <- infiniteIO(2)(csTwo)
      _ <- infiniteIO(22)(csTwo) // won't print
    } yield ()

//  p3.unsafeRunSync()

  def infiniteIO2(id: Int)(implicit cs: ContextShift[IO]): IO[Fiber[IO, Unit]] = {
    def repeat: IO[Unit] = IO(println(id)).flatMap(_ => IO.shift *> repeat)
    repeat.start
  }

  val p4 =
    for {
      fiber1  <- infiniteIO2(1)(csOne)
      fiber11 <- infiniteIO2(11)(csOne)
      fiber2  <- infiniteIO2(2)(csTwo)
      fiber22 <- infiniteIO2(22)(csTwo)
    } yield ()

  // TODO: cancel start
//  p4.unsafeRunTimed(2.seconds)

}
