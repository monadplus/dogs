package concurrency

import java.io._

import cats.effect.concurrent.{MVar, Semaphore}
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

object CopyFiles {
  private def ioStream[T <: Closeable](file: File, semaphore: Semaphore[IO])(f: File => T): Resource[IO, T] =
    Resource.make {
      IO(f(file))
    } { inStream =>
      semaphore.withPermit {
        IO(inStream.close()).handleErrorWith(_ => IO.unit)
      }
    }

  private def inputOutputStreams(in: File,
                                 out: File,
                                 semaphore: Semaphore[IO]): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- ioStream(in, semaphore)(new FileInputStream(_: File))
      outStream <- ioStream(out, semaphore)(new FileOutputStream(_: File))
    } yield (inStream, outStream)

  private def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1)
        IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else
        IO.pure(acc)
    } yield count

  private def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  //      Same as withPermit
  //      _ <- semaphore.acquire
  //      _ <- semaphore.release
  def copy(origin: File, destination: File)(implicit concurrent: Concurrent[IO]): IO[Long] =
    for {
      semaphore <- Semaphore[IO](1)
      count <- inputOutputStreams(origin, destination, semaphore).use {
        case (in, out) =>
          semaphore.withPermit(transfer(in, out))
      }
    } yield count
}

// Exercise:
//  - Finally tagless
//  - Don't use semaphore

object CopyFiles2 {

  case class Token(dummy: Boolean = false) extends AnyVal

  type Semaphore[F[_]] = MVar[F, Token]

  implicit def MVarSyntax[F[_]](semaphore: Semaphore[F]): MVarOps[F] =
    new MVarOps[F](semaphore)

  final class MVarOps[F[_]](val self: Semaphore[F]) extends AnyVal {
    def withPermit[A](t: F[A])(implicit F: Bracket[F, Throwable]): F[A] =
      F.bracket(self.put(Token()))(_ => t)(_ => self.take >> ().pure[F])
  }

  private def ioStream[F[_]: Sync, T <: Closeable](file: File, semaphore: Semaphore[F])(f: File => T): Resource[F, T] =
    Resource.make {
      Sync[F].delay { f(file) }
    } { inStream =>
      semaphore.withPermit {
        Sync[F]
          .delay { inStream.close() }
          .handleErrorWith(_ => ().pure[F])
      }
    }

  private def inputOutputStreams[F[_]: Sync](in: File,
                                             out: File,
                                             semaphore: Semaphore[F]): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- ioStream(in, semaphore)(new FileInputStream(_: File))
      outStream <- ioStream(out, semaphore)(new FileOutputStream(_: File))
    } yield (inStream, outStream)

  private def transmit[F[_]: Sync](origin: InputStream,
                                   destination: OutputStream,
                                   buffer: Array[Byte],
                                   acc: Long): F[Long] =
    for {
      amount <- Sync[F].delay(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1)
        Sync[F].delay(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else
        acc.pure[F]
    } yield count

  private def transfer[F[_]: Sync](origin: InputStream, destination: OutputStream): F[Long] =
    for {
      buffer <- Sync[F].delay(new Array[Byte](1024 * 10))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def copy[F[_]: Concurrent](origin: File, destination: File): F[Long] =
    for {
      semaphore <- MVar[F].empty[Token]
      count <- inputOutputStreams(origin, destination, semaphore).use {
        case (in, out) =>
          semaphore.withPermit(transfer(in, out))
      }
    } yield count
}

object ConcurrencyExamples extends IOApp {
  //  import CopyFiles._
  import CopyFiles2._

  private def askConfirmation: IO[Boolean] =
    for {
      _ <- IO(println("File already exists! Do you want to override it: yes or no ?"))
      resp <- IO(scala.io.StdIn.readLine())
    } yield resp == "yes"

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Error: need origin and destination files"))
      else IO.unit
      orig = new File(args(0))
      dest = new File(args(1))
      _ <- if (orig == dest)
        IO.raiseError(new IllegalArgumentException("Error: origin and destination files files are the same"))
      else
        IO.unit
      isConfirmed <- if (dest.exists())
        askConfirmation
      else
        IO.pure(true)
      count <- if (isConfirmed)
        copy[IO](orig, dest)
      else
        IO.pure(0)
      _ <- IO(println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}"))
    } yield ExitCode.Success
}
