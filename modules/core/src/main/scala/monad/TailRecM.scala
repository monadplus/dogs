package monad

import cats._
import cats.data.Writer

import scala.annotation.tailrec
//import cats.implicits._

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
}
