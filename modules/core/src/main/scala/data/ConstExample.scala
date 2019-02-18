package data

import cats._
import cats.data._
import cats.implicits._

/**
  * Cons[A, B](getConst: A)
  * B is a phantom type
  */
object ConstExample extends App {
  trait Lens0[S, A] {
    def get(s: S): A

    def set(s: S, a: A): S

    def modify(s: S)(f: A => A): S =
      set(s, f(get(s)))

    def modifyOption(s: S)(f: A => Option[A]): Option[S] =
      f(get(s)).map(a => set(s, a))

    def modifyList(s: S)(f: A => List[A]): List[S] =
      f(get(s)).map(a => set(s, a))
  }

  trait Lens[S, A] {
    def modifyF[F[_]: Functor](s: S)(f: A => F[A]): F[S]

    def get(s: S): A = {
      val const: Const[A, S] = modifyF[Const[A, ?]](s)(a => Const(a))
      const.getConst
    }

    def set(s: S, a: A): S =
      modify(s)(_ => a)

    def modify(s: S)(f: A => A): S =
      modifyF[Id](s)(f)
  }

  // ----------------------------
  // ----------------------------
  // ----------------------------

  // traverse-ing over a collection with an effectful function
  // is more general than traversing over a collection to reduce
  // it down to a single value.

  trait Traverse[F[_]] extends Foldable[F] {
    def traverse[G[_]: Applicative, A, X](fa: F[A])(f: A => G[X]): G[F[X]]

    // When Const[A,B] A is a monoid Const forms an applicative.
    override def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = {
      val const: Const[B, F[Nothing]] = traverse[Const[B, ?], A, Nothing](fa)(a => Const(f(a)))
      const.getConst
    }
  }
}
