package mononocle

/**
 *  Lens where the zoomed element may not exist
 *
 *  Optional[S, A]: S represents the Product and A  the optional element.
 */
object OptionalExampe {
  import monocle.Optional

  val head = Optional[List[Int], Int] {
    case Nil     => None
    case x :: xs => Some(x)
  } { x =>
    {
      case Nil     => Nil
      case _ :: xs => x :: xs
    }
  }

  val xs = List(1, 2, 3)
  val ys = List.empty[Int]

  // .nonEmpty check if zoomed not optional
  head.nonEmpty(xs) // true
  head.nonEmpty(ys) // false

  head.getOption(xs)   // Some(1)
  head.getOrModify(xs) // x \/ xs
  head.set(5)(xs)      // List(5,2,3)
  head.modify(_ + 1)(xs)
  head.modifyOption(_ + 1)(xs)

  // Laws

  class OptionalLaws[S, A](optional: Optional[S, A]) {

    def getOptionSet(s: S): Boolean =
      optional.getOrModify(s).fold(identity, optional.set(_)(s)) == s

    def setGetOption(s: S, a: A): Boolean =
      optional.getOption(optional.set(a)(s)) == optional.getOption(s).map(_ => a)

  }
}
