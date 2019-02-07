package mononocle

// A Lens is an optic used to zoom inside a Product,
//   e.g. case class, Tuple, HList or even Map.

// Lenses have two type parameters generally called S
// and A: Lens[S, A] where S represents the Product
// and A an element inside of S.

object LensExample {
  case class Address(streetNumber: Int, streetName: String)
  val address  = Address(69, "Espigol")
  val address2 = Address(24, "Josep estivill")

  import monocle.Lens
  val streetNumber: Lens[Address, Int] =
    Lens[Address, Int](_.streetNumber)(n => a => a.copy(streetNumber = n))

  import monocle.macros.GenLens
  val streetNumber2: Lens[Address, Int] =
    GenLens[Address](_.streetNumber)

  streetNumber.set(streetNumber.get(address))(address2)
  streetNumber.modify(_ + 1)

  def neighbors(n: Int): List[Int] =
    if (n > 0) List(n - 1, n + 1) else List(n + 1)

  import scalaz.std.list._

  streetNumber.modifyF(neighbors _)(address)
  // List(address(n - 1), address(n + 1))

  import scalaz.std.scalaFuture._
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._

  def updateNumber(n: Int): Future[Int] = Future.successful(n + 1)
  streetNumber.modifyF(updateNumber _)(address)

  case class Person(name: String, age: Int, address: Address)
  val john = Person("John", 20, address)

  val personAddress = GenLens[Person](_.address)

  personAddress.composeLens(streetNumber).get(john)
  personAddress.composeLens(streetNumber).set(2)(john)

  // Lens Generation

  val personStreetNumber = GenLens[Person](_.address.streetNumber)
  personStreetNumber.get(john)
  personStreetNumber.set(2)(john)

//   TODO: macro doesn't work !

//   import monocle.macros.Lenses

//   @Lenses case class Point(x: Int, y: Int)
//   @Lenses("_") case class Circle(radius: Int)
//   val p = Point(0, 1)
//   Point.x.get(p)

//   val c = Circle(1)
//   Circle._radius.get(p)

  def getSet[S, A](l: Lens[S, A], s: S): Boolean =
    l.set(l.get(s))(s) == s

  def setGet[S, A](l: Lens[S, A], s: S, a: A): Boolean =
    l.get(l.set(a)(s)) == a
}
