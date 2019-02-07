package mononocle.typeclasses

import monocle.Lens
import monocle.Optional
import monocle.macros.GenLens
import scalaz.NonEmptyList

/**
 * Typeclass that defines a [[monocle.Prism]] between an `S` and its head `A` and tail `S`
 * @tparam S source of [[monocle.Prism]] and tail of [[monocle.Prism]] target
 * @tparam A head of [[monocle.Prism]] target, `A` is supposed to be unique for a given `S`
 */
object consExample extends App {
  case class Street(name: String)
  case class City(streets: List[Street])

  val barcelona = City(
    List(
      Street("Zamora")
    )
  )

  import monocle.function.Cons._

  val streetsOfCity: Lens[City, List[Street]] = GenLens[City](_.streets)
  val firstStreet: Optional[City, Street]     = streetsOfCity.composeOptional(headOption)
  val setStreet                               = firstStreet.set(Street("Diagonal"))
  println(s"City: ${setStreet(barcelona)}")

  case class Mouse(name: String)
  case class Cat(name: String, catches: NonEmptyList[Mouse] /* He is a good predator */ )
  val cat1 = Cat("Darwin", NonEmptyList(Mouse("rattata")))
  println(s"Cat: $cat1")

  import monocle.function.Cons1._

  val catFirstMouseOptionalFirstLetter: Optional[Cat, Char] =
    GenLens[Cat](_.catches).composeLens(head).composeLens(GenLens[Mouse](_.name)).composeOptional(headOption)
  val cat2 = catFirstMouseOptionalFirstLetter.modify(_.toUpper)(cat1)
  println(s"Modified cat: $cat2")
}
