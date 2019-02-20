package data

import cats.arrow.FunctionK
import cats.~>

object FunctionKExample extends App {
  val first0: Function1[List[Int], Option[Int]] = l => l.headOption

  trait MyFunction1[A, B] {
    def apply(a: A): B
  }

  trait MyFunctionK[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  /*_*/
  val first: FunctionK[List, Option] = λ[FunctionK[List, Option]](_.headOption)
  val first2: List ~> Option = λ[List ~> Option](_.headOption)

  type ErrorOr[A] = Either[String, A]
  val errorOrFirst: FunctionK[List, ErrorOr] =
    λ[FunctionK[List, ErrorOr]](_.headOption.toRight("Empty list !"))
  /*_*/
}
