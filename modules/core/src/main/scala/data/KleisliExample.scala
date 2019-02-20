package data

import cats._, cats.data._, cats.implicits._

object KleisliExample extends App {

  /*

  val parse: String => Option[Int] =
    s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

  val reciprocal: Int => Option[Double] =
    i => if (i != 0) Some(1.0 / i) else None

  parse.andThen(reciprocal)
   */

  val parse: Kleisli[Option, String, Int] =
    Kleisli((s: String) => if (s.matches("-?[0-9]+")) Some(s.toInt) else None)

  val reciprocal: Kleisli[Option, Int, Double] =
    Kleisli((i: Int) => if (i != 0) Some(1.0 / i) else None)

  // Binds the effects
  val parseAndReciprocal: Kleisli[Option, String, Double] =
    reciprocal.compose(parse)

  /*
      Method    | Constraint on `F[_]`
      --------- | -------------------
      andThen   | FlatMap
      compose   | FlatMap
      flatMap   | FlatMap
      lower     | Monad
      map       | Functor
      traverse  | Applicative

   */

  // We can define a FlatMap instance for Kleisli if the F[_] we chose has a FlatMap instance

  implicit def kleisliFlatMap[F[_], Z](implicit F: FlatMap[F]): FlatMap[Kleisli[F, Z, ?]] =
    new FlatMap[Kleisli[F, Z, ?]] {
      def flatMap[A, B](fa: Kleisli[F, Z, A])(f: A => Kleisli[F, Z, B]): Kleisli[F, Z, B] =
        Kleisli(z => fa.run(z).flatMap(a => f(a).run(z)))

      def map[A, B](fa: Kleisli[F, Z, A])(f: A => B): Kleisli[F, Z, B] =
        Kleisli(z => fa.run(z).map(f))

      def tailRecM[A, B](a: A)(f: A => Kleisli[F, Z, Either[A, B]]) =
        Kleisli[F, Z, B]({ z =>
          FlatMap[F].tailRecM(a) { f(_).run(z) }
        })
    }

  /*
      Instances depend on

      Type class     | Constraint on `F[_]`
      -------------- | -------------------
      Functor        | Functor
      Apply          | Apply
      Applicative    | Applicative
      FlatMap        | FlatMap
      Monad          | Monad
      Arrow          | Monad
      Split          | FlatMap
      Strong         | Functor
      SemigroupK*    | FlatMap
      MonoidK*       | Monad

   */

  // Configuration

  case class DbConfig(url: String, user: String, pass: String)
  trait Db
  object Db {
    val fromDbConfig: Kleisli[Option, DbConfig, Db] = ???
  }

  case class ServiceConfig(addr: String, port: Int)
  trait Service
  object Service {
    val fromServiceConfig: Kleisli[Option, ServiceConfig, Service] = ???
  }

  case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

  class App(db: Db, service: Service)

  def appFromAppConfig: Kleisli[Option, AppConfig, App] =
    for {
      db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
      sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
    } yield new App(db, sv)
}
