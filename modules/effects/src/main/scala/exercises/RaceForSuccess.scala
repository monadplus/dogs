package exercises

import cats.effect.{ExitCode, IOApp}

// Requirements:
// 1.- The function should run requests in parallel.
// 2.- The function should wait for the first request to complete successfuly.
// 3.- Once a first request has completed, everything that is still in-flight must be cancelled.
// 4.- If all requests have failed, all errors should be reported for better debugging.

// Bonus:
// B1.- Avoid using runtime checking for CompositeException (including pattern matching on it).
// B2.-  If returned IO is cancelled, all in-flight requests should be properly cancelled as well.
// B3.-  Refactor function to allow generic effect type to be used, not only cats’ IO. (e.g. anything with Async or Concurrent instances).
// B4.- Refactor function to allow generic container type to be used (e.g. anything with Traverse or NonEmptyTraverse instances).
//       - Don’t use toList. If you have to work with lists anyway, might as well push the conversion responsibility to the caller.
//       - If you want to support collections that might be empty (List, Vector, Option), the function must result in a failing IO/F when passed an empty value.

object RaceForSuccess extends IOApp {
  import cats._, cats.data._, cats.implicits._
  import cats.effect._, cats.effect.implicits._, cats.effect.concurrent._

  import scala.util.Random
  import scala.concurrent.duration._
  import scala.util._

  case class Data(source: String, body: String)

  def provider(name: String)(implicit timer: Timer[IO]): IO[Data] = {
    val proc = for {
      dur <- IO { Random.nextInt(500) }
      _   <- IO.sleep { (100 + dur).millis }
      _   <- IO { if (Random.nextBoolean()) throw new Exception(s"Error in $name") }
      txt <- IO { Random.alphanumeric.take(16).mkString }
    } yield Data(name, txt)

    proc.guaranteeCase {
      case ExitCase.Completed => IO { println(s"$name request finished") }
      case ExitCase.Canceled  => IO { println(s"$name request canceled") }
      case ExitCase.Error(ex) => IO { println(s"$name errored") }
    }
  }

  case class CompositeException(ex: NonEmptyList[Throwable]) extends Exception("All race candidates have failed")

  def raceToSuccess[A](ios: NonEmptyList[IO[A]]): IO[A] = 
    ???

  val methods: NonEmptyList[IO[Data]] =
    NonEmptyList
      .of(
        "memcached",
        "redis",
        "postgres",
        "mongodb",
        "hdd",
        "aws"
      )
      .map(provider)

      // Using racePair, try folding/reducing the list: 
      // race previous result with attempt, then, if we got a
      //  successful (as in, Right) result from one, cancel the 
      // other and return the result. Otherwise, fall back to 
      // the second one, all while accumulating the errors. 
      // The result should be something like Either[List[Throwable], A].
      //  Then transform list into an exception and use .rethrow to lift it back to IO.    
  def run(args: List[String]) =
    for {
      _ <- IO(println("TODO"))
    } yield ExitCode.Success
}
