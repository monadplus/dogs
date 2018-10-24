package free

import cats.free._

// Source: Composable application architecture with reasonably price monads
// https://www.youtube.com/watch?v=M258zVn4m2M&t=55s
object ComposableApplication {

  /* Bank transfer
  User must be authorized.
  If insufficient funds, log the attempt, raise  an error.
  Otherwise update both accounts
   */

  type Account       = String
  type User          = String
  type Authorization = String
  type Logger        = String
  type ErrorHandler  = String
  type Storage       = String

  def transfer(amount: Long,
               from: Account,
               to: Account,
               user: User,
               auth: Authorization,
               log: Logger,
               err: ErrorHandler,
               store: Storage): Unit = ???

  // Idea: return a description of what we want to do
  /*
  Instruction is the coproduct of:
    - log something
    - fail with an error
    - authorize a user to do something
    - read from storage
    - write to storage
    - interact with the user
   */

  sealed trait Instruction
  def transfer(amount: Long, from: Account, to: Account, user: User): List[Instruction] = ???

  // User interaction
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String)   extends Interact[Unit]
  // this does not work
  val prgAsList = List(Ask("First name?"), Ask("Last name?"), Tell("Hello, ???"))
  // we want somethings like (a monad !)

//  val prg = for {
//    x <- Ask("first name")
//    y <- Ask("last name")
//    _ <- Tell(s"Hello $x, $y")
//  } yield ()

  // Interact data type can't behave like a monad as long as the type is fixed
  // We must use the Free monad

  val prg: Free[Interact, Unit] =
    for {
      first <- Free.liftF(Ask("What's your first name?"))
      last  <- Free.liftF(Ask("What's your last name?"))
      _     <- Free.liftF(Tell(s"Hello, $first $last"))
    } yield ()

//  FlatMapped(
//    Suspend(Ask("What's your first name?")),
//    first =>
//      FlatMapped(Suspend(Ask("What's your last name?")),
//                 last => FlatMapped(Suspend(Tell(s"Hello, $first $last")), _ => Pure(())))
//  )

  // Running Free

  // S: compile-time language
  // M: run-time language
  //   final def foldMap[M[_]](f: FunctionK[S, M])(implicit M: Monad[M]): M[A] =
  //    M.tailRecM(this)(_.step match {
  //      case Pure(a) => M.pure(Right(a))
  //      case Suspend(sa) =>k M.map(f(sa))(Right(_))
  //      case FlatMapped(c, g) => M.map(c.foldMap(f))(cc => Left(g(cc)))
  //    })

  case class IO[A](run: () => A) { self =>
    def flatMap[B](fb: A => IO[B]): IO[B] =
      IO(() => fb(self.run()).run())
  }

  object IO {
    def pure[A](a: => A): IO[A] = IO(() => a)
  }

  //todo: How to make this works (?)
//  object Console extends (Interact ~> IO) {
//    override def apply[A](i: Interact[A]): IO[A] = i match {
//      case Ask(prompt) =>
//        IO.pure(println(prompt)).flatMap(_ => IO.pure(readLine()))
//      case Tell(msg) =>
//        IO.pure(println(msg))
//    }
//  }

  type Tester[A] = Map[String, String] => (List[String], A)

  // todo: same here
//  object Test extends (Interact ~> Tester) {
//    override def apply[A](i: Interact[A]): Tester[A] = i match {
//      case Ask(prompt) =>
//        m =>
//          (List.empty, m(prompt))
//      case Tell(msg) =>
//        _ =>
//          (List(msg), ())
//    }
//  }
}
