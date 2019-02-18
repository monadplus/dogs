package data

import cats._, cats.data._, cats.implicits._

object EvalExample extends App {

  /**
    * Eval.now (eager and memoized)
    * Eval.defer (lazy and memoized)
    * Eval.always (lazy)
    */
  def factorial(n: Long): Eval[Long] =
    if (n == 0) Eval.now(1L)
    else {
      println(n)
      Eval.now(n).flatMap { n =>
        factorial(n - 1).map(_ * n)
      }
    }

  factorial(10000).value
}
