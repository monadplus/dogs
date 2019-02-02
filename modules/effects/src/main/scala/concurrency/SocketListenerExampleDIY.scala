package concurrency

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.concurrent.{ExecutorService, Executors}
import java.lang.Runnable

import cats._
import cats.implicits._
import cats.effect._
import cats.implicits._ // >> (flatMap)
import cats.effect.implicits._
import cats.effect.Sync
import cats.effect.{Concurrent, IO}
import cats.effect.ExitCase.{Canceled, Completed, Error}
import cats.effect.concurrent.MVar

import scala.util._       // Try
import scala.concurrent._ // ExecutionContext

object SocketListenerExampleDIY extends IOApp {

  private def echoProtocol[F[_]: Async: ContextShift](clientSocket: Socket, stopFlag: MVar[F, Unit])(
    implicit ec: ExecutionContext
  ): F[Unit] = {
    def loop(reader: BufferedReader, writer: BufferedWriter): F[Unit] =
      for {
        // async will block. Used for sharing resources.
        line <- Async[F].async { (cb: Either[Throwable, Either[Throwable, String]] => Unit) =>
                 ec.execute(new Runnable() {
                   override def run: Unit = {
                     val line = Try(reader.readLine()).toEither
                     cb(Right(line))
                   }
                 })
               }
        _ <- line match {
              case Right(line) =>
                line match {
                  case "STOP" => stopFlag.put(())
                  case "" => MonadError[F, Throwable].raiseError(new Exception("shine")) //Sync[F].unit
                  // throws exception
                  case _ => Sync[F].delay { writer.write(line); writer.newLine(); writer.flush() } >> loop(reader, writer)
                }
              case Left(e) =>
                for {
                  isEmpty <- stopFlag.isEmpty
                  _ <- if (!isEmpty) Sync[F].unit
                      else MonadError[F, Throwable].raiseError(e)
                } yield ()
            }
      } yield ()

    def reader(clientSocket: Socket): Resource[F, BufferedReader] =
      Resource.make {
        Sync[F].delay(new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
      } { reader =>
        Sync[F].delay(reader.close).handleError(_ => Sync[F].unit)
      }
    def writer(clientSocket: Socket): Resource[F, BufferedWriter] =
      Resource.make {
        Sync[F].delay(new BufferedWriter(new PrintWriter(clientSocket.getOutputStream())))
      } { reader =>
        Sync[F].delay(reader.close).handleError(_ => Sync[F].unit)
      }

    def readerWriter(clientSocket: Socket): Resource[F, (BufferedReader, BufferedWriter)] =
      for {
        reader <- reader(clientSocket)
        writer <- writer(clientSocket)
      } yield (reader, writer)

    readerWriter(clientSocket).use {
      case (reader, writer) =>
        loop(reader, writer)
    }
  }

  def serve[F[_]: Concurrent: ContextShift](serverSocket: ServerSocket, stopFlag: MVar[F, Unit])(
    implicit ec: ExecutionContext
  ): F[Unit] = {
    def close(socket: Socket): F[Unit] =
      Sync[F].delay(socket.close).handleError(_ => Sync[F].unit)

    for {
      fiber <- Sync[F]
                .delay(serverSocket.accept()) // throws
                .bracketCase { socket =>
                  echoProtocol(socket, stopFlag) // throws
                    // This will capture .echoProtocol exception 
                    .guarantee(close(socket))
                    .start
                } {
                  // This will capture .accept exception
                  case (socket, exit) =>
                    exit match {
                      case Completed =>
                        Sync[F].unit
                      case Canceled | Error(_) =>
                        close(socket)
                    }
                }
      _ <- (stopFlag.read >> fiber.cancel).start
      _ <- serve(serverSocket, stopFlag)
    } yield ()
  }

  def server[F[_]: Concurrent: ContextShift](serverSocket: ServerSocket): F[ExitCode] = {
    val pool: ExecutorService =
      Executors.newCachedThreadPool()
    implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutorService(pool)

    for {
      stopFlag <- MVar[F].empty[Unit]
      fiber    <- serve(serverSocket, stopFlag).start
      _        <- stopFlag.read
      _        <- Sync[F].delay(pool.shutdown()) // throws
      _        <- fiber.cancel.start
    } yield ExitCode.Success
  }

  def run(args: List[String]): IO[ExitCode] =
    IO(new ServerSocket(Try(args(0).toInt).getOrElse(5400))).bracket { socket =>
      server[IO](socket) >> IO.pure(ExitCode.Success)
    } { socket =>
      IO.delay(socket.close).handleErrorWith(_ => IO.unit)
    }
}
