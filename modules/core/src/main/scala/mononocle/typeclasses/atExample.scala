package mononocle.typeclasses

import monocle.Lens
import monocle.function.At
import monocle.function.At._
import monocle.macros.GenLens

/**
 * At is a type class that defines a Lens from an S to an A at an index I.
 * Can add, remove and modify an index
 * Use Index for a less powerful version than At
 */
object atExample extends App {
  type DayOfWeek = String
  case class Cooker(name: String)
  case class Restaurant(cookers: Map[DayOfWeek, Cooker])
  val restaurant = Restaurant(
    Map(
      "Monday"  -> Cooker("John"),
      "Tuesday" -> Cooker("Antonio")
    )
  )

  // Add
  GenLens[Restaurant](_.cookers).composeLens(at("Wednesday")).set(Some(Cooker("Paco")))(restaurant)
  // Remove
  GenLens[Restaurant](_.cookers).composeLens(at("Wednesday")).set(None)(restaurant)
}
