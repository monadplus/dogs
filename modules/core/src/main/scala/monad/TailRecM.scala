package monad

import cats._
import cats.data.Writer

import scala.annotation.tailrec
import cats.implicits._

object TailRecM extends App {

  case class LongProduct(value: Long) extends AnyVal
  object LongProduct {
    implicit val longProductMonoid: Monoid[LongProduct] = new Monoid[LongProduct] {
      override def empty: LongProduct =
        LongProduct(1L)
      override def combine(x: LongProduct, y: LongProduct): LongProduct =
        LongProduct(x.value * y.value)
    }
  }

  def powWriter5(x: Long, exp: Long)(implicit m: Monoid[LongProduct]): Writer[LongProduct, Unit] =
    FlatMap[Writer[LongProduct, ?]].tailRecM(exp) {
      case 0   => Writer(m.empty, Right(()))
      case exp => Writer(LongProduct(x), Left(exp - 1))
    }

  def powWriter(x: Long, exp: Long)(implicit m: Monoid[LongProduct]): Writer[LongProduct, Unit] =
    exp match {
      case 0 => Writer(m.empty, ())
      case _ =>
        for {
          _ <- Writer(LongProduct(x), ())
          _ <- powWriter(x, exp - 1)
        } yield ()
    }

  // powWriter will blow the stack eventually
  // let's implement it using a tailRec function

  @tailrec
  def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
    case None           => None
    case Some(Left(a))  => tailRecM(a)(f)
    case Some(Right(b)) => Some(b)
  }

  // it is very similar to the previous implementation
  def powWriter2(x: Long, exp: Long): Writer[LongProduct, Unit] =
    FlatMap[Writer[LongProduct, ?]].tailRecM(exp) {
      case 0 => Writer.value[LongProduct, Either[Long, Unit]](Right(()))
      case m =>
        for {
          _   <- Writer.tell(LongProduct(x))
          res <- Writer.value[LongProduct, Either[Long, Unit]](Left(m - 1))
        } yield res
    }

  println(powWriter5(2, 4).run._1.value)

}

object TailRecMDIY {
  type Log = Vector[String]

  def powWriter3(x: Long, exp: Long): Writer[Log, Long] = exp match {
    case 0 => 1L.writer(Vector(s"Start of computation. Current value: ${1L}"))
    case _ =>
      for {
        res  <- powWriter3(x, exp - 1)
        next = x * res
        _    <- Vector(s"Computing exp: $exp. Current value: $next").tell
      } yield next
  }

  def powWriter4(x: Long, exp: Long): Writer[Log, Long] =
    FlatMap[Writer[Log, ?]].tailRecM[(Long, Long), Long]((exp, 1L)) {
      case (0, acc) =>
        acc.asRight[(Long, Long)].writer(Vector(s"End of computation. Value: $acc"))
      case (m, acc) =>
        for {
          _   <- Vector(s"Computing.. $m steps left >> Current value: $acc").tell
          res <- Writer.value[Log, Either[(Long, Long), Long]](Left(m - 1 -> x * acc))
        } yield res
    }
}
