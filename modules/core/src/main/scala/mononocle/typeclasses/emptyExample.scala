package mononocle.typeclasses

import monocle.{Optional, Prism}
import monocle.macros.GenLens

/**
 * Typeclass that defines a [[Prism]] from an `S` and its empty value
 *
 * @tparam S source of [[Prism]]
 */
object emptyExample extends App {
  import monocle.function.Empty._

  case class Dog(name: String) extends AnyVal
  case class Man(name: String, age: Int, dog: Option[Dog])
  val man        = Man("John", 20, None)
  val manWithDog = Man("Howard", 30, Some(Dog("Bella")))

  val noDog: Optional[Man, Unit] = GenLens[Man](_.dog).composePrism(empty)
  noDog.getOption(man)
  noDog.getOption(manWithDog)
}
