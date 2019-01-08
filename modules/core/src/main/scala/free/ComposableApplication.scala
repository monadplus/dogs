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

  type Account = String
  type UserID  = String
  case class User(userId: UserID) extends AnyVal
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

  //todo: this algebra is broken ...
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

  // New Algebra: Security algebra

  // Problem: check if user is authenticated.
  // yes => tell secrets
  // no  => tell to go away

  type Password   = String
  type Permission = String

  sealed trait Auth[A]

  case class Login(u: UserID, p: Password)         extends Auth[User]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  // Can we do this ? Not yet !

//  val prg2: Free[???, Unit] = for {
//    uid <- Ask("What's your ID?")
//    pwd <- Ask("What's your password")
//    user <- Login(uid, pwd)
//    b <- HasPermission(user, "KnowTheSecret")
//    _ <- if (b) Tell("secret")
//    else Tell("Go away!")
//  } yield ()

  case class Coproduct[F[_], G[_], A](value: Either[F[A], G[A]])

  type App[A] = Coproduct[Interact, Auth, A]

  // Now we need a type class to lift either F or G into the Coproduct
  sealed trait Inject[F[_], G[_]] {
    def inj[A](sub: F[A]): G[A]
  }

  object Inject {
    // Identity Function
    implicit def ref1[F[_]]: Inject[F, F]                        = ???
    implicit def left[F[_], G[_]]: Inject[F, Coproduct[F, G, ?]] = ???
    implicit def right[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, ?]] =
      ???
  }

  def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): Free[G, A] =
    Free.liftF(I.inj(fa))

  class Interacts[F[_]](implicit I: Inject[Interact, F]) {
    def tell(msg: String): Free[F, Unit] =
      lift(Tell(msg))
    def ask(prompt: String): Free[F, String] =
      lift(Ask(prompt))
  }
  class Auths[F[_]](implicit I: Inject[Auth, F]) {
    def login(u: UserID, p: Password): Free[F, User] =
      lift(Login(u, p))
    def hasPermission(u: User, p: Permission): Free[F, Boolean] =
      lift(HasPermission(u, p))
  }

  // end result
  def prg[F[_]](implicit I: Interacts[F], A: Auths[F]): Free[F, Unit] = {
    import I._; import A._
    for {
      uid  <- ask("What's your ID?")
      pwd  <- ask("What's your password")
      user <- login(uid, pwd)
      b    <- hasPermission(user, "KnowTheSecret")
      _    <- if (b) tell("secret") else tell("Go away!")
    } yield ()
  }

  // How do we run the Free Monad Coproduct

}
