package traverse

import cats._
import cats.data._
import cats.implicits._

object ValidatedTraverseExample extends App {

  sealed trait Error
  case class NonFatalError(e: String) extends Error
  case object FatalError              extends Error

  implicit val SemigroupError = new Semigroup[Error] {
    override def combine(x: Error, y: Error): Error = (x, y) match {
      case (NonFatalError(e), NonFatalError(e2)) => NonFatalError(e ++ e2)
      case (_, _)                                => FatalError
    }
  }

  val error: Validated[Error, String]   = NonFatalError("error").invalid
  val success: Validated[Error, String] = "cool".valid
  val res                               = List(error, error).sequence
  val res2                              = List(error, success).sequence
  println(res)
  println(res2)
}
