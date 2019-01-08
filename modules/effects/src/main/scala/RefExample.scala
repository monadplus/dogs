/* cats.effect.concurrent brings:
    Ref - pure mutable reference
    Deferred - a purely functional alternative to Promise
    Semaphore - an access control tool
    MVar - a mutable location that can be empty, useful as synchronization and communication channel
 */

import cats.{Id, Parallel}
import cats.implicits._
import cats.effect._
import concurrent._

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global

object RefExample extends IOApp {
  val program = for {
    ref        <- Ref[IO].of(1)
    printValue = ref.get.flatMap(i => IO(println(s"Current value is $i")))
    _          <- printValue
    _          <- ref.update(_ + 1)
    _          <- printValue
    _          <- ref.update(_ * 2)
    _          <- printValue
  } yield ()

//  Won't print 42
//  val ref = Ref[IO].of(0)
//
//  for {
//    _ <- ref.flatMap(_.update(_ + 42)) // Ref[IO].of(0).flatMap(_.update(_ + 42))
//    x <- ref.flatMap(_.get) // Ref[IO].of(0).flatMap(_.get)
//    _ <- IO(println(s"The value is $x"))
//  } yield ()

  def periodicReader(ref: Ref[IO, Int]): IO[Unit] =
    IO.sleep(1.second) >>
      ref.get.flatMap(i => IO(println(s"Current value is $i"))) >> periodicReader(ref)

  def periodicIncrementer(ref: Ref[IO, Int]): IO[Unit] =
    IO.sleep(750.millis) >> ref.update(_ + 1) >> periodicIncrementer(ref)

  val program2 = for {
    ref  <- Ref[IO].of(0)
    read <- periodicReader(ref).start
    incr <- periodicIncrementer(ref).start
    _    <- IO.sleep(10.seconds)
    _    <- read.cancel
    _    <- incr.cancel
  } yield ()

  // ----------------------------------------

  object StopIteration extends Exception

  def mkIterator[A](seq: Stream[A]): IO[IO[A]] =
    for {
      ref <- Ref[IO].of(seq)
      next = ref.get.flatMap {
        case x #:: xs => ref.set(xs).as(x)
        case _        => IO.raiseError[A](StopIteration)
      }
    } yield next

  val program3 = for {
    next <- mkIterator(Stream(1, 2, 3))
    a    <- next
    b    <- next
    c    <- next
  } yield (c, b, a)

  // Shabby behaviour
  mkIterator(Stream(1, 2, 3)).flatMap(next => next.map(_.toString()))
//  def writeToDb(i: Int): IO[Boolean] = IO { println(s"Wrote $i to DB"); true }
//  val nextBool = next.flatMap(writeToDb) // iterates over Ints, performs a `writeToDb` for each and gives a `Boolean` back.
//  (nextBool *> nextBool *> nextBool).unsafeRunSync()
//  for {
//    next <- mkIterator(Stream(1, 2, 3))
//  } yield {
//    val everyOdd   = next *> next
//    val nextTriple = (next, next, next).tupled
//    next.replicateA(6).parSequence
//  }

  //-----------------------------------------------
  // Memoization

  def someMethod[A](io: IO[A]): IO[IO[A]] =
    for {
      ref <- Ref[IO].of(io)
      _   <- ref.update(_.flatTap(a => ref.set(IO.pure(a))))
    } yield ref.get.flatten

  val exec = IO { println("Hey!"); 42 }
  val program4 = for {
    io <- someMethod(exec)
    x  <- io
    y  <- io
  } yield (x, y) // Prints Hey!, then (42,42)

  override def run(args: List[String]): IO[ExitCode] =
//    program *> IO.pure(ExitCode.Success)
//    program2 *> IO.pure(ExitCode.Success)
    program3.flatMap { o =>
      IO { println(s"output: $o") }
    } *> IO.pure(ExitCode.Success)
}
