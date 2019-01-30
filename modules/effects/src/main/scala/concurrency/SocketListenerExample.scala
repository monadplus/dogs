package concurrency

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.concurrent.{Executors, ExecutorService}
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

import scala.util._ // Try
import scala.concurrent._ // ExecutionContext

// Replies to each text message from a client sending back that same message.
// When the client sends an empty line its connection is shutdown by the server.
object SocketListenerExample extends IOApp {

  private def echoProtocol[F[_]: Async: ContextShift](clientSocket: Socket, stopFlag: MVar[F, Unit])(
    implicit 
    // Option 2: F: ApplicativeError[F, Throwable],
    ec: ExecutionContext
  ): F[Unit] = {

    def evalOn[A] = 
      implicitly[ContextShift[F]].evalOn[A](ec) _

    def loop(reader: BufferedReader, writer: BufferedWriter): F[Unit] = {
      for {
        // Option 2: lineE <- evalOn(F.attempt(Sync[F].delay(reader.readLine())))
        // Option 1: async is blocking ! 
        lineE <- Async[F].async { (cb: Either[Throwable, Either[Throwable, String]] => Unit) =>
          ec.execute(new Runnable() {
            override def run(): Unit = {
              val res = Try(reader.readLine()).toEither
              cb(Right(res))
            }
          })
        }
        _ <- lineE match {
              case Right(line) =>
                line match {
                  case "STOP" => stopFlag.put(())
                  case ""     => Sync[F].unit
                  case _ =>
                    Sync[F].delay { writer.write(line); writer.newLine(); writer.flush() } >> loop(reader, writer)
                }
              case Left(e) =>
                for {
                  isEmpty <- stopFlag.isEmpty
                  _ <- if (!isEmpty) Sync[F].unit // Graceful shutdown
                      else Sync[F].raiseError(e)
                } yield ()
            }
      } yield ()
    }

    def reader(clientSocket: Socket): Resource[F, BufferedReader] =
      Resource.make {
        Sync[F].delay(new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
      } { reader =>
        Sync[F].delay(reader.close()).handleErrorWith(_ => Sync[F].unit)
      }

    def writer(clientSocket: Socket): Resource[F, BufferedWriter] =
      Resource.make {
        Sync[F].delay(new BufferedWriter(new PrintWriter(clientSocket.getOutputStream())))
      } { writer =>
        Sync[F].delay(writer.close()).handleErrorWith(_ => Sync[F].unit)
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

  def serve[F[_]: Concurrent: ContextShift](serverSocket: ServerSocket, stopFlag: MVar[F, Unit])
                             (implicit ec: ExecutionContext): F[Unit] = {
    def close(socket: Socket): F[Unit] =
      Sync[F].delay(socket.close()).handleErrorWith(_ => Sync[F].unit)

    for {
      fiber <- Sync[F]
            .delay(serverSocket.accept())
            .bracketCase { socket =>
              Sync[F].delay(println(s"Connection established: ${socket.getInetAddress}")).flatMap { _ =>
                echoProtocol(socket, stopFlag)
                  .guarantee(close(socket))
                  .start // Option 2: >> socket.pure[F]
              }
            } { (socket, exit) =>
              exit match {
                case Completed           => Sync[F].unit
                case Error(_) | Canceled => close(socket)
              }
            }
      // Option 2: _ <- (stopFlag.read >> close(socket)).start
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
      stopFlag    <- MVar[F].empty[Unit]
      serverFiber <- serve(serverSocket, stopFlag).start
      _           <- stopFlag.read // Blocks until "STOP" is send
      _           <- Sync[F].delay(pool.shutdown())
      _           <- serverFiber.cancel.start
    } yield ExitCode.Success
  }

  def run(args: List[String]): IO[ExitCode] = {

    def close[F[_]: Sync](socket: ServerSocket): F[Unit] =
      Sync[F].delay(socket.close()).handleErrorWith(_ => Sync[F].unit)

    IO(new ServerSocket(args.headOption.map(_.toInt).getOrElse(5432)))
      .bracket { serverSocket =>
        IO(println("ServerSocket started...")) >> server[IO](serverSocket) >> IO.pure(ExitCode.Success)
      } { serverSocket =>
        close[IO](serverSocket) >> IO(println("Server finished"))
      }
  }
}
