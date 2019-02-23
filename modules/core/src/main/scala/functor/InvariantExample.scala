package functor

/*
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
 */
object InvariantExample extends App {
  import cats._
  import cats.implicits._

  import java.util.Date

  def longToDate: Long => Date = new Date(_)
  def dateToLong: Date => Long = _.getTime

  implicit val semigroupDate: Semigroup[Date] =
    Semigroup[Long].imap(longToDate)(dateToLong)

  val today: Date = longToDate(1449088684104l)
  val timeLeft: Date = longToDate(1449088684104l)

  println(s"Today: $today")
  println(s"TimeLeft: $timeLeft")
  // Combine a date makes no sense...
  println(s"Combine: ${today |+| timeLeft}")
}
