package trampoline

import scala.annotation.tailrec

/**
  * Trampoline is a way to make non-tail recursive functions stack-safe
  *
  * Author: Rúnar Bjarnason
  * Source: http://blog.higher-order.com/assets/trampolines.pdf
  */
object TrampolineExplained extends App {

  /**
    * During the execution, we will build a structure which is essentially
    * equivalent as the call stack, except that it is built on the heap.
    */
  // Monad
  sealed trait TailRec[+A] {
    def map[B](f: A => B): TailRec[B] = flatMap(f.andThen(Return(_)))
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
  }

  final case class Return[A](a: A) extends TailRec[A]
  final case class Suspend[A](resume: () => TailRec[A]) extends TailRec[A]
  final case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  // verbose on purpose
  def unsafeFac(n: Int): Int =
    if (n == 0) return 1
    else {
      val x = unsafeFac(n - 1)
      return n * x
    }

  /*
    We then rewrite the original recursive function using the TailRec type, in the following manner:

    - If the original function returns an A, the new function should return a TailRec[A].
    - Each return in the original function should be wrapped in a Return.
    - Each recursive call in the original function should be wrapped in a Suspend.
    - Things we do after the recursive call (in this case, multiply the result by n) should be wrapped in a FlatMap.
   */

  def fac(n: Int): TailRec[Int] =
    if (n == 0) Return(1)
    else
      Suspend(() => fac(n - 1))
        .flatMap(x => Return(n * x))

  /*

  The returns (which pop stack frames) and recursive calls (which push stack frames)
  are gone - replaced by our own data types, Return and Suspend.
  This gives us control of how the new factorial function is executed.
  The Suspend class wraps a thunk (a function that takes no parameter).
  This makes it lazy: when we create a Suspend, the function it wraps is not evaluated.

   */

  // To execute a trampolined function (i.e., to extract the A out of a TailRec[A]),
  // we use the following tail-recursive run function:

  /*_*/
  @tailrec
  def run[A](tr: TailRec[A]): A = tr match {
    case Return(a)  => a
    case Suspend(r) => run(r())
    case FlatMap(sub, f) =>
      sub match {
        case Return(a)        => run(f(a))
        case Suspend(r)       => run(r().flatMap(f(_)))
        case FlatMap(sub2, g) => run(sub2.flatMap { g(_).flatMap(f) })
      }
  }
  /*_*/

//  unsafeFac(100000) // StackOverflow
  run(fac(100000))

  /*
  Execution of run(fac(5))

  FlatMap(Suspend(() => fac(4)), x => Return(5 * x))
  run(FlatMap(Suspend(() => fac(4)), x => Return(5 * x)))
  run(FlatMap(fac(4), x => Return(5 * x)))
  run(FlatMap(FlatMap(Suspend(() => fac(3)), x => Return (4 * x)), x => Return(5 * x)))
    ...

  In the end we have build the following structure:

  FlatMap(
    Return(1),
    x => Flatmap(
      Return(1 * x), x => Flatmap(
        Return(2 * x), x => Flatmap(
          Return(3 * x), x => Flatmap(
            Return(4 * x), x => Return(5 * x)
          )
        )
      )
    )
  )
   */

  def unsafeEven(n: Int): Boolean =
    if (n == 0) true
    else unsafeOdd(n - 1)

  def unsafeOdd(n: Int): Boolean =
    if (n == 0) false
    else unsafeEven(n - 1)

  /*
  These two functions are in fact tail recursive, but they are mutually tail recursive:
    they are defined in terms of each other.

  Scala cannot optimize for mutual tail recursions due to limitations of JVM (by contrast, Haskell can,
  so these two functions are stack-safe in Haskell), so passing a large n to either function will cause StackOverflowError.
   */

  def even(n: Int): TailRec[Boolean] =
    if (n == 0) Return(true)
    else Suspend(() => odd(n - 1))

  def odd(n: Int): TailRec[Boolean] =
    if (n == 0) Return(false)
    else Suspend(() => even(n - 1))

  // **** Fibonnaci ****

  def unsafeFib(n: Int): Int =
    if (n <= 1) n
    else unsafeFib(n - 2) + unsafeFib(n - 1)

  def fib(n: Int): TailRec[Int] =
    if (n <= 1) Return(n)
    else {
      for {
        x <- Suspend(() => fib(n - 2))
        y <- Suspend(() => fib(n - 1))
      } yield x + y
    }

  sealed trait Tree[A] {
    def label: A
  }
  final case class Leaf[A](label: A) extends Tree[A]
  final case class Node[A](label: A, children: List[Tree[A]]) extends Tree[A]

  def unsafeTreeMap[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(a)           => Leaf(f(a))
    case Node(a, children) => Node(f(a), children.map(unsafeTreeMap(_, f)))
  }

  def sequence[A](ltt: List[TailRec[A]]): TailRec[List[A]] =
    // (_ :: a) will construct the list backwards. :+ is slower
    ltt.reverse.foldLeft(Return(Nil): TailRec[List[A]]) { (tla, ta) =>
      tla.flatMap(la => ta.map(_ :: la)) // Same as: ta.map(((_: A) :: (_: List[A])).curried).flatMap(tla.map)
    }

  def treeMap[A, B](tree: Tree[A], f: A => B): TailRec[Tree[B]] = tree match {
    case Leaf(a) => Return(Leaf(f(a)))
    case Node(a, children) =>
      val ltt: List[TailRec[Tree[B]]] = children.map(child => Suspend(() => treeMap(child, f)))
      val tlt: TailRec[List[Tree[B]]] = sequence(ltt)
      tlt.flatMap(lt => Return(Node(f(a), lt)))
  }

  val tree: Tree[Int] = Node(1, List(Node(2, List(Leaf(3))), Leaf(0)))

  println(tree)
  println(run(treeMap(tree, (_: Int).toString)))

  // The key reason why trampolined functions are stack-safe is because Suspend is lazy,
  // in other words, the recursion happens in a lazy structure. Generally speaking,
  // lazy recursions tend to be stack-safe, even if they are not tail recursions.
  //
  // For example, the following function:

  def func(x: Int): Stream[Int] = x #:: func(x + 1)
  func(1).take(1000000).toList // with a large n will not cause StackOverflowError

  // The execution simply trampolines between func and take.
}
