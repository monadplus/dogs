package mononocle.typeclasses

import monocle.Lens
import monocle.function.At

/**
 * At is a type class that defines a Lens from an S to an A at an index I.
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
  val cookerAt = At[Restaurant, DayOfWeek, Cooker](d => office => office.cookers(d))(
    d => e => o => o.copy(cookers = o.cookers ++ Map(d -> e))
  )
  val setCookerMonday: Lens[Restaurant, Cooker] = cookerAt.at("Monday")
  println(setCookerMonday.set(Cooker("Jose"))(restaurant))
}
