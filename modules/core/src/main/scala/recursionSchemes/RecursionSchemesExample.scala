package recursionSchemes

import cats.Functor
import cats.implicits._

object RecursionSchemesExample extends App {
  /** Recursion schemes is all about fixpoint types */

  /** Fixed-point types */
  // c is a fixed point of the function f(x) if f(c) = c. This means f(f(...f(c)...)) = fn(c) = c
  // We can think of a higher-order function fix that computes a fixed point of its argument: fix(f) == x such that f(x) == x.
  // By extension, fix(f) == f(fix(f)) holds.

  // By analogy, a fixed-point type of a higher-kinded type F[_] is some type T such that F[T] == T
  // We can also have a type Fix[F[_]], such that Fix[F] == F[Fix[F]]

  // Fixed-point types are interesting, because they can represent a recursive structure of any desired depth as a single type:
  //   Fix[F] == F[Fix[F]] == F[F[Fix[F]]] == ...

  case class Fix[F[_]](unfix: F[Fix[F]])

  /************ Anamorphism **************/
  //  Anamorphism is one of the basic types of recursion schemes.
  //  It generalizes unfold, meaning you can use it to create a recursive structure based on a recursive type.

  // Simplified stack (on heap)
  sealed trait StackR
  final case class DoneR(result: Int = 1) extends StackR
  final case class MoreR(stack: StackR, next: Int) extends StackR

  // We can use the following function to generate a structure representing the call stack for calculating the factorial of n:

  def unfoldStackR: Int => StackR =
    n => if (n > 0) MoreR(unfoldStackR(n - 1), n) else DoneR()

  unfoldStackR(5) // MoreR(MoreR(MoreR(MoreR(MoreR(DoneR(1),1),2),3),4),5)

  // Anamorphism is a type of recursion scheme, which takes a function of type A => F[A] (also known as a Coalgebra) where F is a functor,
  // and returns a function of type A => Fix[F], that takes an A and unfolds it into a recursive structure, Fix[F].

  def ana[F[_]: Functor,  A](coalgebra: A => F[A]): A => Fix[F] =
    a => Fix(coalgebra(a) map ana(coalgebra))

  // That is, we only need to supply a coalgebra, A => F[A] which does not need to be recursive,
  // in order to generate the recursive structure.
  // This is what we mean by “factoring recursion out”: your coalgebra only needs to contain the core business logic,
  // and the recursion is taken care of by ana.

  sealed trait Stack[A]
  final case class Done[A](result: Int) extends Stack[A]
  final case class More[A](a: A, next: Int) extends Stack[A]

  // To use recursion schemes, Stack must be a functor, so we add a functor instance for Stack
  object Stack {
    implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
      override def map[A, B](sa: Stack[A])(f: A => B): Stack[B] =
        sa match {
          case Done(result) => Done(result)
          case More(a, next) => More(f(a), next)
        }
    }

    def done[A](result: Int = 1): Stack[A] = Done(result)
    def more[A](a: A, next: Int): Stack[A] = More(a, next)
  }

  import Stack._

  val stackCoalgebra: Int => Stack[Int] =
    n => if (n > 0) more(n - 1, n) else done()

  ana[Stack, Int](stackCoalgebra).apply(5) // Fix(More(Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4)),5))

  /***************** Catamorphism **************/

  def cata[F[_] : Functor, A](algebra: F[A] => A): Fix[F] => A =
    fix => algebra(fix.unfix map cata(algebra))

  // Explicit recursion with StackR
  def foldStackR: StackR => Int = {
    case DoneR(result) => result
    case MoreR(acc, next) => foldStackR(acc) * next
  }

  // Explicit recursion with Fix[Stack]
  def foldFixStack: Fix[Stack] => Int =
    _.unfix match {
      case Done(result) => result
      case More(fix, next) => foldFixStack(fix) * next
    }

  (unfoldStackR andThen foldStackR)(5) // 120
  (ana(stackCoalgebra) andThen foldFixStack)(5) // 120

  val stackAlgebra: Stack[Int] => Int = {
    case Done(result) => result
    case More(acc, next) => acc * next
  }

  cata[Stack, Int](stackAlgebra).apply(Fix(More(Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4)),5))) // 120

  val factorial: Int => Int = ana(stackCoalgebra) andThen cata(stackAlgebra)

  println(factorial(5)) // 120

  /** Hylomorphism:  composition of anamorphism and catamorphism */

  // !! avoid creating a large Fix structure in memory
  def hyloSimple[F[_] : Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    ana[F,A](g) andThen cata[F, B](f)

  def hylo[F[_] : Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    a => f(g(a) map hylo(f)(g))

  hylo(stackAlgebra)(stackCoalgebra).apply(5) // 120

  /** Paramorphism */

  // Paramorphism is also a generalization of fold.
  // It is an extension of catamorphism, and offers more power.
  def para[F[_] : Functor, A](f: F[(Fix[F], A)] => A): Fix[F] => A =
    fix => f(fix.unfix map {fix => fix -> para(f).apply(fix)})

  // We can implement cata in terms of para:
  def cataViaPara[F[_] : Functor, A](f: F[A] => A): Fix[F] => A =
    para(((_: F[(Fix[F], A)]).map(_._2)) andThen f)

  // Paramorphism is more powerful than catamorphism in the sense that in the algebra f,
  // we not only have an F[A] to work with, but we also have an F[Fix[F]], which means we
  // have access to the Fix structure that yields the A when being folded.

  // Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4))

  // This is not particularly useful for calculating factorials using our Stack type.
  // However, paramorphism does allow us to calculate factorials using the following simpler type, NatR, representing natural numbers:

  sealed trait Nat[+A]
  final case object Zero extends Nat[Nothing]
  final case class Succ[A](a: A) extends Nat[A]

  object Nat {
    implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
      override def map[A, B](na: Nat[A])(f: A => B): Nat[B] =
        na match {
          case Succ(a) => Succ(f(a))
          case Zero => Zero
        }
    }
  }

  val natAlgebra: Nat[Int] => Int = {
    case Zero => 1
    case Succ(n) => n + 1
  }

  val natAlgebraPara: Nat[(Fix[Nat], Int)] => Int = {
    case Zero => 1
    case Succ((fix, acc)) => cata(natAlgebra).apply(fix) * acc
  }

  val natCoAlebra: Int => Nat[Int] =
    n => if (n == 0) Zero  else Succ(n-1)

  val factorialViaPara = ana(natCoAlebra) andThen para(natAlgebraPara)
  factorialViaPara(5) // 120

  /**  Apomorphism  */

  // Apomorphism is the dual of paramorphism, and is an extension of anamorphism.

  def apo[F[_] : Functor, A](f: A => F[Either[Fix[F], A]]): A => Fix[F] =
    a => Fix(f(a) map {
      case Left(fix) => fix
      case Right(aa) => apo(f).apply(aa)
    })

  // Note that in addition to reversing the arrows in para, we also change F[(Fix[F], A)] to F[Either[Fix[F], A]],
  // since the dual of a pair (product type) is an Either (sum type).

  // Not surprisingly, ana can be implemented in terms of apo:

  def anaViaApo[F[_] : Functor, A](f: A => F[A]): A => Fix[F] =
    apo(f andThen (_ map (_.asRight[Fix[F]])))

  // In apomorphism, the recursion can be terminated
  // either by visiting a base case, or if f returns a Left containing a Fix[F].

  // Example: terminate recursion on n = 3

  val lastThreeSteps: Fix[Stack] = Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3))

  val stackCoalgebraApo: Int => Stack[Either[Fix[Stack], Int]] =
    n => if (n > 3) more(n - 1, n).map(Right(_)) else lastThreeSteps.unfix.map(Left(_))

  apo(stackCoalgebraApo).apply(5) //the recursion stops at n = 3, where lastThreeSteps is returned, instead of continuing the recursion.

  /** Histomorphism: more powerful than paramorphism */

  // Histomorphism operates on an enhanced version of Fix, called Cofree,
  // where each node in the structure is annotated by some value.

  final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  // Recall that in catamorphism, at each step of the fold, you only have the value of the current fold, F[A].
  // In paramorphism, you additionally have access to the structure that generated that value, F[Fix[F]].
  // And in histomorphism, additionally, you also have the history of the values generated by the fold so far,
  // or the history of the computation if you will.

  // In the factorial example, in the step corresponding to n = 5, in catamorphism we have More(acc=24, next=5);
  // in paramorphism we also have the structure that generated 24, i.e., Fix(More(Fix(More(Fix(More(Fix(Done(1)),2)),3)),4));
  // in histomorphism we additionally have the history of the generated values, i.e., 1, 1, 2, 6, 24.
  // Each of these values is encoded as the head of a Cofree structure, together with the corresponding node, which is in the tail.
  def histo[F[_] : Functor, A](f: F[Cofree[F, A]] => A): Fix[F] => A = {
    // where each node is annotated by the value generated by folding the corresponding Fix[F] structure:
    def toCofree: Fix[F] => Cofree[F, A] =
      fix => Cofree(head = histo(f).apply(fix), tail = fix.unfix map toCofree)

    fix => f(fix.unfix map toCofree)
  }

  /** Dynamorphism */
  // Dynamorphism is the composition of anamorhpism and histomorphism.

  // composition of ana and histo
  def dynaSimple[F[_] : Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B =
    ana(g) andThen histo(f)

  // Best way
  def dyna[F[_] : Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B = {
    val cofree: F[Cofree[F, B]] => Cofree[F, B] =
      fc => Cofree(f(fc), fc)
    a => hylo(cofree)(g).apply(a).head
  }

  /** Futumorphism: dual of histo */
  // The dual of Cofree is, unsurprisingly, the following Free type:

  // Each Cofree has a recursive structure tagged with a value of type A,
  // while each Free has either a recursive structure, or a tag with a value of type A.
  sealed trait Free[F[_], A]
  final case class Continue[F[_], A](a: A) extends Free[F, A]
  final case class Combine[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

  object Free {
    def continue[F[_], A](a: A): Free[F, A] = Continue(a)
    def combine[F[_], A](fa: F[Free[F, A]]): Free[F, A] = Combine(fa)
  }

  import Free._

  // more powerful than apomorphism

  // Recall that in apomorphism, given a value of A, you either choose to continue
  // the recursion by returning a Right, or choose to stop the recursion by returning a Left.

  // In futumorphism, you can also choose to continue the recursion by returning a Continue
  // or stop by returning a Combine. Additionally, you will be able to unfold multiple steps at a time,
  // which you cannot do with apomorphism.
  def futu[F[_] : Functor, A](f: A => F[Free[F, A]]): A => Fix[F] = {
    def toFix: Free[F, A] => Fix[F] = {
      case Continue(a) => futu(f).apply(a)
      case Combine(fa) => Fix(fa map toFix)
    }

    a => Fix(f(a) map toFix)
  }

  val firstThreeSteps: Stack[Free[Stack, Int]] = more(combine(more(continue(3), 4)), 5)
  val stackCoalgebraFutu: Int => Stack[Free[Stack, Int]] =
    n =>
      if (n == 5) firstThreeSteps
      else if (n > 0) more(n - 1, n) map continue
      else done() map continue

  futu[Stack, Int](stackCoalgebraFutu).apply(5)
}
