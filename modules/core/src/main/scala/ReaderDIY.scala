// Source: https://www.youtube.com/watch?v=ZasXwtTRkio
object ReaderDIY extends App {
  case class Reader[A, B](run: A => B) {
    def apply(a: A): B                  = run(a)
    def map[C](f: B => C): Reader[A, C] = Reader(a => f(run(a)))
    def flatMap[C](f: B => Reader[A, C]): Reader[A, C] =
      Reader(a => f(run(a))(a))
  }

  object Reader {
    final private[Reader] class PurePartiallyApplied[A](val dummy: Boolean = false) extends AnyVal {
      def apply[B](value: B): Reader[A, B] = Reader(_ => value)
    }
    def pure[A]: PurePartiallyApplied[A] = new PurePartiallyApplied[A]
  }

  sealed trait Connection
  case object TestConnection extends Connection
  type Password = String
  def selectPassword(userId: String): Reader[Connection, Password]                    = ???
  def updatePassword(userId: String, newPassword: Password): Reader[Connection, Unit] = ???
  def changePwd(userId: String, oldPwd: Password, newPwd: Password): Reader[Connection, Boolean] =
    for {
      pwd <- selectPassword(userId)
      isUpdated <- if (oldPwd == pwd) for {
                    _ <- updatePassword(userId, newPwd)
                  } yield true
                  else Reader.pure[Connection](false)
    } yield isUpdated

  changePwd("arnauabella", "1234", "4321")(TestConnection)
}
