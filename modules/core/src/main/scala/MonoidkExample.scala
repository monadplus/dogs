
/**
  * MonoidK[F] allows two F[A] values to be combined, for any A.
  * It also means that for any A, there is an “empty” F[A] value.
  */
object MonoidkExample extends App {
  import cats._
  import cats.implicits._

  SemigroupK[Option].combineK[String](Some("Hello"), Some("Bye")) // Some("hello")
  SemigroupK[Option].combineK[String](Some("Hello"), MonoidK[Option].empty) // Some("Hello")
  SemigroupK[Option].combineK[String](MonoidK[Option].empty, Some("Hello")) // Some("Hello")
}
