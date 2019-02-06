package monocle_test

/**
 *  Generalisation of Optional to several targets (Traverse)
 *
 *  Traversal[T[]: Traverse, T[S], A]
 */
object TraversalExample {
  import monocle.Traversal
  import scalaz.std.list._ // to get the Traverse instance for List

  val xs = List(1, 2, 3, 4, 5)

  val eachL = Traversal.fromTraverse[List, Int]
  eachL.set(0)(xs) //  List(0, 0 ...)
  eachL.modify(_ + 1)(xs)

  // Traversal is also a Fold
  eachL.getAll(xs)      // xs ...
  eachL.headOption(xs)  // Some(1)
  eachL.find(_ > 3)(xs) // Some(4)
  eachL.all(_ % 2 == 0)(xs)

  // Smart constructor for traversal structures
  case class Point(id: String, x: Int, y: Int)
  val points = Traversal.apply2[Point, Int](_.x, _.y)((x, y, p) => p.copy(x = x, y = y))

  points.set(5)(Point("bottom-left", 0, 0)) // Point("bottom-left", 5, 5)

  import monocle.Traversal
  import scalaz.Applicative
  import scalaz.std.map._
  import scalaz.syntax.traverse._
  import scalaz.syntax.applicative._

  def filterKey[K, V](predicate: K => Boolean): Traversal[Map[K, V], V] =
    new Traversal[Map[K, V], V] {
      def modifyF[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.map {
          case (k, v) =>
            k -> (if (predicate(k)) f(v) else v.pure[F])
        }.sequenceU
    }

  val m = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "Four")
  filterKey(_ % 2).modify(_.toUpperCase)(m) // Map(1 -> one, 2 -> TWO, 3 -> three, 4 -> FOUR)
}

object TraversalLaws {
  import monocle.Traversal

  def modifyGetAll[S, A](t: Traversal[S, A], s: S, f: A => A): Boolean =
    t.getAll(t.modify(f)(s)) == t.getAll(s).map(f)

  def composeModify[S, A](t: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean =
    t.modify(g)(t.modify(f)(s)) == t.modify(g.compose(f))(s)
}
