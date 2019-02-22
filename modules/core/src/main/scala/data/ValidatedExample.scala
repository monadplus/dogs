package data

import cats.data.Validated.{Invalid, Valid}
import cats.data._

/**
  * Validated is an applicative functor (mapN) but not a monad
  */
object ValidatedExample extends App {
  import cats.Monad

  // Applicative is defined in terms of Monad

  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](x: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(f.andThen(pure))

    // TODO: using the flatMap derivation will short-circuit and this is not the expected behaviour we need to implement a custom ap:
    /*
     def ap[A, B](ff: Validated[E, (A) => B])(fa: Validated[E, A]): Validated[E, B] =
         fa.ap(ff)(Semigroup[E])

     def ap[EE >: E, B](f: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE, B] = (this, f) match {
      case (Valid(a), Valid(f))       => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e2, e1))
      case (e @ Invalid(_), _)        => e
      case (_, e @ Invalid(_))        => e
    }
     */
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B] =
      flatMap(fa)(a => map(f)(fab => fab(a)))
  }

  implicit def validatedMonad[E]: Monad[Validated[E, ?]] =
    new Monad[Validated[E, ?]] {
      def flatMap[A, B](fa: Validated[E, A])(f: A => Validated[E, B]): Validated[E, B] =
        fa match {
          case Valid(a)       => f(a)
          case i @ Invalid(_) => i
        }

      def pure[A](x: A): Validated[E, A] = Valid(x)

      @annotation.tailrec
      def tailRecM[A, B](a: A)(f: A => Validated[E, Either[A, B]]): Validated[E, B] =
        f(a) match {
          case Valid(Right(b)) => Valid(b)
          case Valid(Left(a))  => tailRecM(a)(f)
          case i @ Invalid(_)  => i
        }
    }
}
