****Contravariant****

Ordering  instances:
def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T]
implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

-------------------------------
****ContravariantMonoidal****

trait ContravariantMonoidal[F[_]] extends Contravariant[F] {
  def unit: F[Unit]

  def product[A, B](fa: F[A], fc: F[B]): F[(A, B)]

  def contramap2[A, B, C](fb: F[B], fc: F[C])(f: A => (B, C)): F[A] =
    contramap(product(fb, fc))(f)
}

-------------------------------
****InvariantMonoidal****

InvariantMonoidal combines Invariant and Semigroupal with the addition of a unit methods

trait InvariantMonoidal[F[_]] {
  def unit: F[Unit]
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}