package data

import cats.data.Ior
import cats.implicits._

/**
  * object Ior extends IorInstances with IorFunctions with IorFunctions2 {
  *   final case class Left[+A](a: A) extends (A Ior Nothing)
  *   final case class Right[+B](b: B) extends (Nothing Ior B)
  *   final case class Both[+A, +B](a: A, b: B) extends (A Ior B)
  * }
  *
  * Short-circuit on left
  * Accumulates on both (only if left is a semigroup)
  *
  *
  * Ior is right-biased,
  */
object IorExample extends App {
  3.rightIor[String]
  "hi".leftIor[String]
  Ior.both("Warning", -1)
  Ior.bothNel("Warning", -1)
  Ior.bothNec("Warning", -1)

  // -------------------------------
  // -------------------------------
  // -------------------------------
  // -------------------------------

  /**
    * When we look at the Monad or Applicative instances of Ior, we can see that they actually requires a Semigroup
    * instance on the left side. This is because Ior will actually accumulate failures on the left side, very similar
    * to how the Validated data type does. This means we can accumulate data on the left side while also being able
    * to short-circuit upon the first right-side-only value. For example, sometimes, we might want to accumulate warnings
    * together with a valid result and only halt the computation on a “hard error”
    */
  import cats.implicits._
  import cats.data.{NonEmptyChain => Nec}

  type Failures = Nec[String]
  case class Username(value: String) extends AnyVal
  case class Password(value: String) extends AnyVal
  case class User(name: Username, pw: Password)

  def validateUsername(u: String): Failures Ior Username =
    if (u.isEmpty)
      Ior.leftNec("Can't be empty")
    else if (u.contains("."))
      Ior.bothNec("Dot in name is deprecated", Username(u))
    else
      Username(u).rightIor

  def validatePassword(p: String): Failures Ior Password =
    if (p.length < 8)
      Ior.leftNec("Password too short")
    else if (p.length < 10)
      Ior.bothNec("Password should be longer", Password(p))
    else
      Password(p).rightIor

  /*_*/
  def validateUser(name: String, password: String): Failures Ior User =
    (validateUsername(name), validatePassword(password)).mapN(User)
  /*_*/

  println(validateUser("John", "password12")) // Right(User(Username(John),Password(password12)))
  println(validateUser("john.doe", "password")) // Both(Chain(Dot in name is deprecated, Password should be longer),User(Username(john.doe),Password
  println(validateUser("", "short")) // Left(Chain(Password too short))
}
