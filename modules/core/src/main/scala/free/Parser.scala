package free

import cats._
import cats.data._
import cats.free.Free

// Source: https://www.youtube.com/watch?v=hmX2s3pe_qk
object Parser {
  /*
  - Take a problem that occurs often
  - Expresss instances of it as sentences in a simple language
  - Solve the problem by interpreting these sentences
   */

  sealed trait Exp {
    def interpret(s: String): (String, Boolean) =
      this match {
        case Lit(l) if s.startsWith(l) =>
          (s drop l.length, true)
        case And(a, b) =>
          val (ns, p) = a interpret s
          if (p) b interpret ns else (s, false)
        case Or(a, b) =>
          val (ns, p) = a interpret s
          if (p) (ns, true) else b interpret s
        case Many(e) =>
          val (ns, p) = e interpret s
          if (p) Many(e) interpret ns else (s, true)
        case _ =>
          (s, false)
      }
  }

  case class Lit(s: String)      extends Exp
  case class And(a: Exp, b: Exp) extends Exp
  case class Or(a: Exp, b: Exp)  extends Exp
  case class Many(e: Exp)        extends Exp

  def toStateMachine(e: Exp): State[String, Boolean] =
    State(e.interpret)

  // fold is the interpreter for option

  // following the fold example

  sealed trait Exp2[T] {
    def fold[A](lit: T => A, and: (A, A) => A, or: (A, A) => A, many: A => A): A = {
      def go(x: Exp2[T]): A = x match {
        case Lit2(t)    => lit(t)
        case And2(a, b) => and(go(a), go(b))
        case Or2(a, b)  => or(go(a), go(b))
        case Many2(e)   => many(go(e))
      }
      go(this)
    }
  }

  case class Lit2[T](t: T)                   extends Exp2[T]
  case class And2[T](a: Exp2[T], b: Exp2[T]) extends Exp2[T]
  case class Or2[T](a: Exp2[T], b: Exp2[T])  extends Exp2[T]
  case class Many2[T](e: Exp2[T])            extends Exp2[T]

  // Folding replaces all the instruction in
  // the program with instructions in different language

  // Boolean expression language (constant: true|false is missing)
  //exp.fold(identity[Boolean], _ && _, _ || _, !_)
  sealed trait Exp3[V] {
    def fold[A](lit: Boolean => A,
                and: (A, A) => A,
                not: A => A,
                or: (A, A) => A,
                lookup: V => A): A = {
      def go(x: Exp3[V]): A = x match {
        case Lit3(t)    => lit(t)
        case And3(a, b) => and(go(a), go(b))
        case Not3(e)    => not(go(e))
        case Or3(a, b)  => or(go(a), go(b))
        case Var3(v)    => lookup(v)
      }
      go(this)
    }
  }
  case class Lit3[A](b: Boolean)             extends Exp3[A]
  case class And3[A](a: Exp3[A], b: Exp3[A]) extends Exp3[A]
  case class Not3[A](e: Exp3[A])             extends Exp3[A]
  case class Or3[A](a: Exp3[A], b: Exp3[A])  extends Exp3[A]
  case class Var3[A](v: A)                   extends Exp3[A]

  def evaluate[A](e: Exp3[A], env: A => Boolean): Boolean =
    e.fold[Boolean](identity[Boolean], _ && _, !_, _ || _, env)

  // this is a flatMap
  def replace[A, B](e: Exp3[A], env: A => Exp3[B]): Exp3[B] =
    e.fold[Exp3[B]](Lit3(_), And3(_, _), Not3(_), Or3(_, _), env)
}

object ParserRevisited {
  // names are the one used in the book
  sealed trait Exp[F[_], A]
  case class TerminalExp[F[_], A](a: A)               extends Exp[F, A]
  case class NonTerminalExp[F[_], A](s: F[Exp[F, A]]) extends Exp[F, A]

  // this is called  the free monad
  sealed trait Free[F[_], A] {
    def foldMap[G[_]: Monad](f: F ~> G): G[A] =
      this match {
        case Return(a) =>
          Monad[G].pure(a)
        case Suspend(s) =>
          Monad[G].flatMap(f(s))(_.foldMap(f))
      }
  }
  case class Return[F[_], A](a: A)              extends Free[F, A]
  case class Suspend[F[_], A](s: F[Free[F, A]]) extends Free[F, A]

  sealed trait BoolAlg[A]
  case class Lit[A](b: Boolean) extends BoolAlg[A]
  case class And[A](a: A, b: A) extends BoolAlg[A]
  case class Or[A](a: A, b: A)  extends BoolAlg[A]
  case class Not[A](a: A)       extends BoolAlg[A]

  type BoolExp[A] = Free[BoolAlg, A]

  // Option monad using Free
  type Trivial[A] = Unit
  type Option[A]  = Free[Trivial, A]
}

sealed trait CharToyAlgebra[+Next]
object CharToyAlgebra {

  // The algebra
  case class CharOutput[Next](c: Char, n: Next) extends CharToyAlgebra[Next]
  case class CharBell[Next](n: Next)            extends CharToyAlgebra[Next]
  case class CharDone()                         extends CharToyAlgebra[Nothing]

  // Free algebra
  def output(c: Char): Free[CharToyAlgebra, Unit] =
    Free.liftF[CharToyAlgebra, Unit](CharOutput(c, ()))
  def bell: Free[CharToyAlgebra, Unit] =
    Free.liftF[CharToyAlgebra, Unit](CharBell(()))
  def done: Free[CharToyAlgebra, Unit] =
    Free.liftF[CharToyAlgebra, Unit](CharDone())
  def pure[A](a: A): Free[CharToyAlgebra, A] =
    Free.pure(a)

  implicit val charToyAlgebraFunctor: Functor[CharToyAlgebra] = new Functor[CharToyAlgebra] {
    override def map[A, B](fa: CharToyAlgebra[A])(f: A => B): CharToyAlgebra[B] =
      fa match {
        case CharOutput(c, n) => CharOutput(c, f(n))
        case CharBell(n)      => CharBell(f(n))
        case CharDone()       => CharDone()
      }
  }
  // Create an interpreter
  def showProgram[R: Show](p: Free[CharToyAlgebra, R])(implicit s: Show[Char]): String =
    p.fold[String](
      { r: R =>
        "return " + Show[R].show(r) + "\n"
      }, {
        case CharOutput(c, n) => "output: " + Show[Char].show(c) + "\n" + showProgram(n)
        case CharBell(n)      => "bell" + "\n" + showProgram(n)
        case CharDone()       => "done\n"
      }
    )
}
