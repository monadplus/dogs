/**
  *  Data types that are actually of the same structure, but instead have instances of Applicative. E.g. Either and Validated.
  *  Use both in conjunction with each other.
  */
object ParallelExample extends App {

  /*
  trait Parallel[M[_], F[_]] {
      def sequential: F ~> M
      def parallel: M ~> F

      def applicative: Applicative[F]
      def monad: Monad[M]

      override def apply: Apply[F] = applicative
      override def flatMap: FlatMap[M] = monad
  }
   */
  import cats.data._
  import cats.implicits._

  case class Name(value: String)
  case class Age(value: Int)
  case class Person(name: Name, age: Age)

  def parse(s: String): Either[NonEmptyList[String], Int] = {
    if (s.matches("-?[0-9]+")) Right(s.toInt)
    else Left(NonEmptyList.one(s"$s is not a valid integer."))
  }

  def validateAge(a: Int): Either[NonEmptyList[String], Age] = {
    if (a > 18) Right(Age(a))
    else Left(NonEmptyList.one(s"$a is not old enough"))
  }

  def validateName(n: String): Either[NonEmptyList[String], Name] = {
    if (n.length >= 8) Right(Name(n))
    else Left(NonEmptyList.one(s"$n Does not have enough characters"))
  }

  def parsePersonBoilerplate(ageString: String, nameString: String) =
    for {
      age <- parse(ageString)
      // Manually Validated ~> Either
      person <- (validateName(nameString).toValidated, validateAge(age).toValidated)
        .mapN(Person)
        .toEither
    } yield person

  def parsePerson(ageString: String, nameString: String) =
    for {
      age <- parse(ageString)
      person <- (validateName(nameString), validateAge(age)).parMapN(Person)
    } yield person

  // traverse
  val parSeq = List(NonEmptyChain("Error 1").asLeft, NonEmptyChain("Error 2").asLeft, 12.asRight).parSequence
  println(s"Traverse using applicative instance: $parSeq") // Left(Chain(Error 1, Error 2))

  println((List(1, 2, 3), List(4, 5, 6)).mapN(_ -> _)) // Cartesian product: List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))
  println((List(1, 2, 3), List(4, 5, 6)).parMapN(_ -> _)) // Zipper: List(1 -> 4, 2 -> 5, 3 -> 6)


}
