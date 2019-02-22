package data

import cats.data.Validated.Valid
import cats.data._
import cats.implicits._

import scala.concurrent.Future

/**
  * The OneAnd[F[_],A] data type represents a single element of type A that is guaranteed to be present (head) and in addition to this a second
  * part that is wrapped inside an higher kinded type constructor F[_]
  */
object OneAndExample extends App {
  type NonEmptyList[A] = OneAnd[List, A]
  type NonEmptyStream[A] = OneAnd[Stream, A]
}
