package free
import cats.arrow.FunctionK
import cats.data.{Const, Tuple2K}
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import cats.~>
import cats.implicits._

import scala.util.{Failure, Success}

/**
  * Same as free monads but limited.
  *
  * Read FreeMonadExample before jumping to this one.
  */
object FreeApplicativeExample extends App {

  sealed abstract class ValidationOp[A]
  case class Size(size: Int) extends ValidationOp[Boolean]
  case object HasNumber extends ValidationOp[Boolean]

  type Validation[A] = FreeApplicative[ValidationOp, A]

  def size(size: Int): Validation[Boolean] = lift(Size(size))
  val hasNumber: Validation[Boolean] = lift(HasNumber)

  // FreeApplicative doesn't support for-comprehension
  val prog: Validation[Boolean] = (size(5), hasNumber).mapN { case (l, r) => l && r }

  /*_*/
  type FromString[A] = String => A
  object compiler extends (ValidationOp ~> FromString) {
    def apply[A](fa: ValidationOp[A]): FromString[A] =
      str =>
        fa match {
          case Size(size) => str.size >= size
          case HasNumber  => str.exists(c => "0123456789".contains(c))
      }
  }
  /*_*/

  val validator: FromString[Boolean] =
    prog.foldMap[FromString](compiler)

  println(s"Validating 1234: ${validator("1234")}")
  println(s"Validating 12345: ${validator("12345")}")

  /*
   Difference from Free

   Recall a key distinction between the type classes Applicative and Monad.
   Applicative captures the idea of independent computations, whereas Monad captures that of dependent computations.
   Put differently Applicative cannot branch based on the value of an existing/prior computation.
   Therefore when using Applicative, we must hand in all our data in one go.

   FreeApplicative.foldMap operates on two stacks:
     - Arg stack: List[FreeApplicative[F, _]]
     - Function stack: List[Fn[G, _, _]](contains a function to be Ap‘d that has already been translated to the target Applicative)

           '1A
           /  \
       arg/    \fun
         /      \
        /        \
       2A        3A
   arg/  \fun arg/  \fun
     /    \     /    \
    4L    5P   6L     7L

   Uses ap to fold right-side nodes (function stack)
   (Not sure) FreeMonad should use flatMap

   */

  // ***** Parallel validator *****

  import cats.data.Kleisli
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  type ParValidator[A] = Kleisli[Future, String, A]

  /*_*/
  object parCompiler extends (ValidationOp ~> ParValidator) {
    def apply[A](fa: ValidationOp[A]): ParValidator[A] = Kleisli { str =>
      fa match {
        case Size(size) => Future { str.size >= size }
        case HasNumber  => Future { str.exists(c => "0123456789".contains(c)) }
      }
    }
  }
  /*_*/

  val parValidator: ParValidator[Boolean] =
    prog.foldMap[ParValidator](parCompiler)

  parValidator("1234").onComplete {
    case Success(validation) =>
      println(s"Validating 1234: $validation")
    case _ =>
      ()
  }

  // ***** Logging *****

  type Log[A] = Const[List[String], A]
  val logCompiler = new FunctionK[ValidationOp, Log] {
    def apply[A](fa: ValidationOp[A]): Log[A] = fa match {
      case Size(size) => Const(List(s"size >= $size"))
      case HasNumber  => Const(List("has number"))
    }
  }

//  Not gonna work on the runtime.
//  val writerCompiler = new FunctionK[ValidationOp, Writer[List[String], ?]] {
//    def apply[A](fa: ValidationOp[A]): Writer[List[String], A] = fa match {
//      case Size(size) => List(s"size >= $size").writer(true).asInstanceOf[Writer[List[String], A]]
//      case HasNumber  => List("has number").writer(true).asInstanceOf[Writer[List[String], A]]
//    }
//  }

  def logValidation[A](validation: Validation[A]): List[String] =
    validation.foldMap[Log](logCompiler).getConst

  println(s"Log program: ${logValidation(prog)}")
  println(s"Log program2: ${logValidation(size(5) *> hasNumber *> size(10))}")

  // ***** Both *****

  // Another useful property Applicatives have over Monads is that given
  // two Applicatives F[_] and G[_], their product type FG[A] = (F[A], G[A]) is also an Applicative.
  // This is not true in the general case for monads.

  type ValidateAndLog[A] = Tuple2K[ParValidator, Log, A]
  val prodCompiler: FunctionK[ValidationOp, ValidateAndLog] = parCompiler.and(logCompiler)
  val prodValidation = prog.foldMap[ValidateAndLog](prodCompiler)
  prodValidation.first("1234").onComplete {
    case Success(validation) =>
      println(s"""Program results:
           | - Validation $validation
           | - Log        ${prodValidation.second.getConst}""".stripMargin)
    case Failure(e) =>
      println(s"Program failed: ${e.getMessage}")
  }
}
