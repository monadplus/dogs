import cats.data.State
import cats.{Eval, Foldable, Traverse}
import cats.implicits._

object Examples {

  // Short-circuit fold with Either
  def get[F[_]: Foldable, A](fa: F[A])(index: Long): Option[A] =
    Foldable[F].foldM[Either[A, ?], A, Long](fa, 0L) { (idx, a) =>
      if (index == idx) Left(a) else Right(idx +  1)
    }.fold(_.some, _ => none)

  def collectFirst[F[_]: Foldable, A, B](fa: F[A])(pf: PartialFunction[A, B]): Option[B] =
    Foldable[F].foldRight(fa, Eval.now(none[B])) { (a, lb) =>
      // Presumably the fastest way to get in and out of a partial function is for a sentinel function to return itself (trick from TraverseOnce)
      val sentinel = new scala.runtime.AbstractFunction1[Any, Any] {def apply(v1: Any): Any = this}
      val x = pf.applyOrElse(a, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) Eval.now(Some(x.asInstanceOf[B]))
      else lb
    }.value

  def mapWithIndex[F[_]: Traverse, A, B](fa: F[A])(f: (A, Int) => B): F[B] =
    fa.traverse(a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value
}