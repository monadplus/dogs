package mononocle.typeclasses

object fieldExample extends App {
  case class Monitor(inches: (Int, Int))
  val monitor = Monitor(inches = (3, 4))

  import monocle.macros.GenLens
  import monocle.function.Field1._
  import monocle.function.Field2._

  val monitorWidth  = GenLens[Monitor](_.inches).composeLens(first)
  val monitorHeight = GenLens[Monitor](_.inches).composeLens(second)

  val monitor2 = monitorWidth.set(10).andThen(monitorHeight.set(18))(monitor)
  println(s"Monitor modified: $monitor2")
}
