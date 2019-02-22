package data

import cats._
import cats.data.Validated.Valid
import cats.data._
import cats.implicits._

import scala.concurrent.Future

/**
  * Nested: nested composition of effects.
  *         But not monads as monads do not stack
  */
object NestedExample extends App {
  val x: Option[Validated[String, Int]] = Some(Valid(123))
  x.map(_.map(_.toString)) // This is tedious

  /*_*/
  val nested: Nested[Option, Validated[String, ?], Int] = Nested(Some(123.valid[String]))
  nested.map(_.toString).value
  /*_*/

  /*

    If F[_] and G[_] are both Functors, then Nested[F, G, ?] is also a Functor (we saw this in action in the example above)
    If F[_] and G[_] are both Applicatives, then Nested[F, G, ?] is also an Applicative
    If F[_] is an ApplicativeError and G[_] is an Applicative, then Nested[F, G, ?] is an ApplicativeError
    If F[_] and G[_] are both Traverses, then Nested[F, G, ?] is also a Traverse

   */

  case class UserInfo(name: String, age: Int)
  case class User(id: String, name: String, age: Int)

  def createUser(userInfo: UserInfo): Future[Either[List[String], User]] =
    Future.successful(Right(User("user 123", userInfo.name, userInfo.age)))

  // Almost all future instances requires an Execution Context
  import scala.concurrent.ExecutionContext.Implicits.global

  def createUsers(userInfos: List[UserInfo]): Future[Either[List[String], List[User]]] =
    // scala: .traverse[Nested[Future, Either[List[String], ?], ?], User]
    userInfos
      .traverse(userInfo => Nested(createUser(userInfo))).value
}
