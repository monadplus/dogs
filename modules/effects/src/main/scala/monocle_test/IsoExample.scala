package monocle_test

/**
 *  * An [[monocle.Iso]] defines an isomorphism between a type S and A
 */
object IsoExample {
  case class Person(name: String, age: Int)

  import monocle.Iso

  val personToTuple = Iso[Person, (String, Int)](p => (p.name, p.age)) { case (name, age) => Person(name, age) }
  personToTuple.get(Person("Zoe", 25))
  personToTuple.reverseGet(("Zoe", 25)) == personToTuple("Zoe", 25)

  def listToVector[A] = Iso[List[A], Vector[A]](_.toVector)(_.toList)
  def vectorToList[A] = listToVector[A].reverse
  listToVector.get(List(1, 2, 3))
  vectorToList.get(Vector(1, 2, 3))

  val stringToList = Iso[String, List[Char]](_.toList)(_.mkString(""))
  stringToList.modify(_.tail)("Hello") // ello

  // Generation

  case class MyString(s: String)
  case class Foo()
  case object Bar

  import monocle.macros.GenIso

  /** Generate an [[Iso]] between a case class `S` and its unique field of type `A`. */
  GenIso[MyString, String].get(MyString("Hello")) // Hello

  /** Generate an [[Iso]] between an object `S` and `Unit`. */
  GenIso.unit[Foo]

  /** Generate an [[Iso]] between a case class `S` and its fields. */
  GenIso.fields[Person].get(Person("John", 30))

  // Laws
  def roundTripOneWay[S, A](i: Iso[S, A], s: S): Boolean =
    i.reverseGet(i.get(s)) == s

  def roundTripOtherWay[S, A](i: Iso[S, A], a: A): Boolean =
    i.get(i.reverseGet(a)) == a
}
