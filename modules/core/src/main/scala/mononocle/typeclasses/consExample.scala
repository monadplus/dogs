package mononocle.typeclasses

import monocle.Lens
import monocle.function.At

/**
 * Typeclass that defines a [[monocle.Prism]] between an `S` and its head `A` and tail `S`
 * @tparam S source of [[monocle.Prism]] and tail of [[monocle.Prism]] target
 * @tparam A head of [[monocle.Prism]] target, `A` is supposed to be unique for a given `S`
 */
object consExample {
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
