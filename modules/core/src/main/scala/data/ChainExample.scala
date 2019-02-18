package data

import cats.kernel.Monoid

/**
  * Chain: constant append and prepend operations (map and traverse still as performance as List | Vector).
  *        -> So next time you write any code that uses List or Vector as a Monoid, be sure to use Chain instead!
  *
  * Implementation:
  *
  * sealed abstract class Chain[+A]
  * final case object Empty extends Chain[Nothing]
  * final case class Singleton[A](a: A) extends Chain[A]
  * final case class Append[A](left: Chain[A], right: Chain[A]) extends Chain[A]
  * final case class Wrap[A](seq: Seq[A]) extends Chain[A]
  *   - wrap Vector and List..
  */
object ChainExample extends App {
  import cats._
  import cats.data._ // Chain, NonEmptyChain, Nel
  import cats.implicits._

//  Chain.nil
  Chain.empty[Int]
  Chain.one(1) ++ Chain.one(2)
  Chain.fromSeq(List(1, 2, 3))
  Chain(1, 2, 3)

//  Monoid[Chain[Int]].empty |+| Chain(1)

  // -------------------------------------------------

  NonEmptyChain(1, 2, 3, 4)
  NonEmptyChain.fromNonEmptyList(NonEmptyList(1, List(2, 3)))
  NonEmptyChain.fromNonEmptyVector(NonEmptyVector(1, Vector(2, 3)))
  NonEmptyChain.one(1)
  NonEmptyChain.fromChain(Chain(1, 2, 3))
  NonEmptyChain.fromSeq(List.empty[Int])
  NonEmptyChain.fromSeq(Vector(1, 2, 3))
  NonEmptyChain.fromChainAppend(Chain(1, 2, 3), 4)
  NonEmptyChain.fromChainAppend(Chain.empty[Int], 1)
  NonEmptyChain.fromChainPrepend(1, Chain(2, 3))

}
