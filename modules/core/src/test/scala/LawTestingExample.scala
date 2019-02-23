import cats.{Eq, Functor}
import org.scalatest.FreeSpec
import org.scalacheck.{Arbitrary, Gen}
import cats.implicits._
import cats.kernel.Semigroup
import cats.tests.CatsSuite

/**
  * TL;DR
  * Gen[T] for generating instances
  * Arbitrary[T] encapsulates this behaviour for a given type (I don't finde it useful)
  * Test using Prop.forAll and then check (you can modify the default parameters)
  * Shrink[T] to minimise failing case
  */
/*
  sealed abstract class Arbitrary[T]() extends scala.AnyRef with scala.Serializable {
    val arbitrary : org.scalacheck.Gen[T]
  }
  object Arbitrary {
    def apply[T](gen: Gen[T]): Arbitrary[T]
  }

  Prop.forAll(implicit Arbitrary[Int])((x: Int) => x + x == x)

  Test Case Minimisation

  val p1 = forAllNoShrink(arbitrary[List[Int]])(l => l == l.distinct)
  val p2 = forAll(arbitrary[List[Int]])(l => l == l.distinct)

  scala> p1.check
  ! Falsified after 11 passed tests:
  > ARG_0 = "List(8, 0, -1, -3, -8, 8, 2, -10, 9, 1, -8)"

  scala> p2.check
  ! Falsified after 4 passed tests:
  > ARG_0 = "List(-1, -1)" (2 shrinks)
 */
sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[A](node: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  implicit val functor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf => Leaf
      case Node(node, left, right) => Node(f(node), map(left)(f), map(right)(f))
    }
  }

  // Semilattices are commutative semigroups whose operation (i.e. combine) is also idempotent.
  implicit def semigroupTree[A: Semigroup]: Semigroup[Tree[A]] = new Semigroup[Tree[A]] {
    def combine(x: Tree[A], y: Tree[A]) = (x, y) match {
      case (Leaf, _) => Leaf
      case (_, Leaf) => Leaf
      case (Node(xp, xLeft, xRight), Node(yp, yLeft, yRight)) =>
        Node(xp |+| yp, xLeft |+| yLeft, xRight |+| yRight)
    }
  }

  implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals

  implicit def arbFoo[A: Arbitrary]: Arbitrary[Tree[A]] = {
    val treeGen: Gen[Tree[A]] =
      Gen.frequency(
        6 -> Gen.const(Leaf),
        4 -> (
          for {
            e <- Arbitrary.arbitrary[A]
          } yield Node(e, Leaf, Leaf)
          )
      )
    Arbitrary(treeGen)
  }
}

class LawTestingExample extends FreeSpec {
  "Tree" - {
    "Functor" - {
      "should obey all ways" in {
        import Tree._
        import cats.laws.discipline._
        FunctorTests[Tree].functor[Int, Int, String].all.check()
      }
    }
  }
}

class LawTestingExample2 extends CatsSuite { // CatsSuite: FunSuite + Matchers + Discipline(two helper checkAll) + ats.implicits._.
  import Tree._
  import cats.kernel.laws.discipline._ // SemigroupTests
  import cats.laws.discipline._ // FunctorTests

  checkAll("Tree[Int].SemigroupLaws", SemigroupTests[Tree[Int]].semigroup)
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}