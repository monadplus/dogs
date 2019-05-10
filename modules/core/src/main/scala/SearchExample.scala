import shapeless.HNil

// source: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/deepsearch.scala

object SearchExample extends App {
  import shapeless._

  // Find the first element in the type that matches the predicate
  trait Searchable[A, Q] {
    def find(p: Q => Boolean)(a: A): Option[Q]
  }

  trait LowPrioritySearchable {
    // We need this for products and
    implicit def genericSearchable[A, L <: HList, Q](
        implicit gen: Generic.Aux[A, L],
        s: Searchable[L, Q]
    ): Searchable[A, Q] =
      new Searchable[A, Q] {
        def find(p: Q => Boolean)(a: A): Option[Q] =
          s.find(p)(gen.to(a))
      }
  }

  object Searchable extends LowPrioritySearchable {
    implicit def hnilSearchable[Q]: Searchable[HNil, Q] =
      new Searchable[HNil, Q] {
        def find(p: Q => Boolean)(a: HNil): Option[Q] = None
      }

    implicit def sameElementSearchable[A]: Searchable[A, A] =
      new Searchable[A, A] {
        def find(p: A => Boolean)(a: A): Option[A] = if (p(a)) Some(a) else None
      }

    // If the H type doesn't match Q, sameElementSearchable won't produce an implicit.
    implicit def hlistSearchable[H, T <: HList, Q](
        implicit h: Searchable[H, Q] = null,
        t: Searchable[T, Q]
    ): Searchable[H :: T, Q] =
      new Searchable[H :: T, Q] {
        def find(p: Q => Boolean)(a: H :: T): Option[Q] =
          Option(h).flatMap(_.find(p)(a.head)).orElse(t.find(p)(a.tail))
      }

    implicit def listSearchable[A, Q](implicit s: Searchable[A, Q]): Searchable[List[A], Q] =
      new Searchable[List[A], Q] {
        def find(p: Q => Boolean)(as: List[A]): Option[Q] =
          as.flatMap(s.find(p)).headOption
      }
  }

  implicit class SearchableOps[A](private val a: A) extends AnyVal {
    def deepFind[Q](p: Q => Boolean)(implicit searchable: Searchable[A, Q]) =
      searchable.find(p)(a)
  }

  // An example predicate:
  val p = (_: String).endsWith("o")

  // On strings:
  assert("hello".deepFind(p) == Some("hello"))
  assert("hell".deepFind(p) == None)

  // On lists:
  assert(List("yes", "maybe", "no").deepFind(p) == Some("no"))

  // On arbitrarily sized and nested tuples:
  assert(("yes", "maybe", ("no", "why")).deepFind(p) == Some("no"))
  assert(("a", ("b", "c"), "d").deepFind(p) == None)

  // On tuples with non-string elements:
  assert((1, "two", (Symbol("three"), '4')).deepFind(p) == Some("two"))

  // Search the same tuple for a specific character instead:
  assert((1, "two", (Symbol("three"), '4')).deepFind((_: Char) == 52) == Some('4'))

  // Our case class:
  case class Foo(a: String, b: String, c: List[String])

  // And it works:
  assert(Foo("four", "three", List("two", "one")).deepFind(p) == Some("two"))
  assert(Foo("a", "b", "c" :: Nil).deepFind(p) == None)
}
