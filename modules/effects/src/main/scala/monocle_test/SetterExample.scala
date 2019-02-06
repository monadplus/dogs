package monocle_test

/**
 * A [[monocle.PSetter]] is a generalisation of Functor map:
 *  - `map:    (A => B) => F[A] => F[B]`
 *  - `modify: (A => B) => S    => T`
 */
object SetterExample extends App {
  import monocle.Setter

  case class Address(streetNumber: Int, streetName: String)
  case class Person(name: String, age: Int, address: Address)

  val sherlock = Person("Sherlock Holmes", 20, Address(221, "Baker Street"))

  val personAge: Setter[Person, Int] = Setter[Person, Int] { f => p =>
    p.copy(age = f(p.age))
  }
  val personAddress: Setter[Person, Address] = Setter[Person, Address] { f => p =>
    p.copy(address = f(p.address))
  }
  val addressNumber: Setter[Address, Int] = Setter[Address, Int] { f => a =>
    a.copy(streetNumber = f(a.streetNumber))
  }
  personAge.modify(_ + 1)(sherlock)
  personAge.set(21)(sherlock)

  personAddress.composeSetter(addressNumber).modify(_ + 1)
  personAddress.composeSetter(addressNumber).set(200)
}

object SetterLaws {
  // TODO express as methods
  // def setIdempotent(s: S, a: A): IsEq[S] =
  //    setter.set(a)(setter.set(a)(s)) <==> setter.set(a)(s)
  //
  //  def modifyIdentity(s: S): IsEq[S] =
  //    setter.modify(identity)(s) <==> s
  //
  //  def composeModify(s: S, f: A => A, g: A => A): IsEq[S] =
  //    setter.modify(g)(setter.modify(f)(s)) <==> setter.modify(g compose f)(s)
  //
  //  def consistentSetModify(s: S, a: A): IsEq[S] =
  //    setter.modify(_ => a)(s) <==> setter.set(a)(s)
}
