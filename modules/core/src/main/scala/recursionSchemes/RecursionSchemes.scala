package recursionSchemes

import cats.Functor
import cats.implicits._

object RecursionSchemes {
  case class Fix[F[_]](unfix: F[Fix[F]])

  final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  sealed trait Free[F[_], A]
  final case class Continue[F[_], A](a: A) extends Free[F, A]
  final case class Combine[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

  object Free {
    def continue[F[_], A](a: A): Free[F, A] = Continue(a)
    def combine[F[_], A](fa: F[Free[F, A]]): Free[F, A] = Combine(fa)
  }

  def ana[F[_]: Functor, A](coalgebra: A => F[A]): A => Fix[F] =
    a => Fix(coalgebra(a).map(ana(coalgebra)))
  // dual of ana
  def cata[F[_]: Functor, A](algebra: F[A] => A): Fix[F] => A =
    fix => algebra(fix.unfix.map(cata(algebra)))
  // composition of anamorphism and catamorphism
  def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    a => f(g(a).map(hylo(f)(g)))
  // powerful version of ana
  def apo[F[_]: Functor, A](f: A => F[Either[Fix[F], A]]): A => Fix[F] =
    a =>
      Fix(f(a).map {
        case Left(fix) => fix
        case Right(aa) => apo(f).apply(aa)
      })
  // powerful version of cata
  def para[F[_]: Functor, A](f: F[(Fix[F], A)] => A): Fix[F] => A =
    fix =>
      f(fix.unfix.map { fix =>
        fix -> para(f).apply(fix)
      })
  // powerful version of para
  def histo[F[_]: Functor, A](f: F[Cofree[F, A]] => A): Fix[F] => A = {
    def toCofree: Fix[F] => Cofree[F, A] =
      fix => Cofree(head = histo(f).apply(fix), tail = fix.unfix.map(toCofree))

    fix =>
      f(fix.unfix.map(toCofree))
  }
  // powerful version of hylo
  def dyna[F[_]: Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B = {
    val cofree: F[Cofree[F, B]] => Cofree[F, B] =
      fc => Cofree(f(fc), fc)
    a =>
      hylo(cofree)(g).apply(a).head
  }

  // dual of dyna
  def futu[F[_]: Functor, A](f: A => F[Free[F, A]]): A => Fix[F] = {
    def toFix: Free[F, A] => Fix[F] = {
      case Continue(a) => futu(f).apply(a)
      case Combine(fa) => Fix(fa.map(toFix))
    }
    a =>
      Fix(f(a).map(toFix))
  }
}
