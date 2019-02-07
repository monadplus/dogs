package mononocle.typeclasses

import monocle.Iso

object eachExample extends App {
  case class Moon(name: String) extends AnyVal
  case class Planet(name: String, moons: Vector[Moon])

  val planet = Planet(
    name = "Saturn",
    moons = Vector(Moon("Tethys"), Moon("Dione"))
  )

  import monocle.Lens
  import monocle.macros.GenLens
  import monocle.macros.GenIso
  import monocle.function.Each._ // implicits ! (have a look)

  val satellites: Lens[Planet, Vector[Moon]] = GenLens[Planet](_.moons)
  val satelliteName: Iso[Moon, String]       = GenIso[Moon, String]
  val planetNames                            = satellites.composeTraversal(each).composeIso(satelliteName)
  println(planetNames.getAll(planet))
}
