package free

import cats.Functor

// Source: https://www.youtube.com/watch?v=ZasXwtTRkio
object Interpreter extends App {
  sealed trait KVSAlg[A]
  case class Put[A](key: String, value: String, a: A) extends KVSAlg[A]
  case class Get[A](key: String, h: String => A)      extends KVSAlg[A]
  case class Delete[A](key: String, a: A)             extends KVSAlg[A]

  def modify0(k: String, f: String => String): KVSAlg[KVSAlg[Unit]] =
    Get(k, v => Put(k, f(v), ()))

  case class Done[F[_]: Functor, A](a: A)             extends Free[F, A]
  case class More[F[_]: Functor, A](k: F[Free[F, A]]) extends Free[F, A]
  // this is actually a monad
  abstract class Free[F[_], A](implicit F: Functor[F]) {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      this match {
        case Done(a) => f(a)
        case More(k) => More(F.map(k)(_ flatMap f))
      }
    def map[B](f: A => B): Free[F, B] =
      flatMap(x => Done(f(x)))
  }

  implicit lazy val kvsFunctor: Functor[KVSAlg] = new Functor[KVSAlg] {
    override def map[A, B](fa: KVSAlg[A])(f: A => B): KVSAlg[B] = fa match {
      case Put(k, v, a) => Put(k, v, f(a))
      case Get(k, h)    => Get(k, x => f(h(x)))
      case Delete(k, a) => Delete(k, f(a))
    }
  }

  type KVS[A] = Free[KVSAlg, A]

  // KVS monad
  def put(k: String, v: String): KVS[Unit] =
    More(Put(k, v, Done(())))
  def get(k: String): KVS[String] =
    More(Get(k, v => Done(v)))
  def delete(k: String): KVS[Unit] =
    More(Delete(k, Done(())))
  def modify(k: String, f: String => String): KVS[Unit] =
    for {
      v <- get(k)
      _ <- put(k, f(v))
    } yield ()

  // KVS interpreter
  def runKVS[A](kvs: KVS[A], table: Map[String, String]): Map[String, String] =
    kvs match {
      case More(Put(k, v, a)) =>
        runKVS(a, table + (k -> v))
      case More(Get(k, h)) =>
        runKVS(h(table(k)), table)
      case More(Delete(k, a)) =>
        runKVS(a, table - k)
      case Done(_) => table
    }
}

object Interpreter0 {
  import Interpreter._

  // Define the algebra
  sealed trait ConsoleAlg[A]
  case class Read[A](r: String => A)   extends ConsoleAlg[A]
  case class Write[A](v: String, a: A) extends ConsoleAlg[A]

  // Make your algebra monadic: Free[Algebra, A]
  type Console[A] = Free[ConsoleAlg, A]

  implicit lazy val ConsoleFunctor: Functor[ConsoleAlg] = new Functor[ConsoleAlg] {
    override def map[A, B](fa: ConsoleAlg[A])(f: A => B): ConsoleAlg[B] =
      fa match {
        case Read(r)     => Read(s => f(r(s)))
        case Write(v, a) => Write(v, f(a))
      }
  }

  def getStr: Console[String] =
    More(Read(s => Done(s)))
  def putStr(v: String): Console[Unit] =
    More(Write(v, Done(())))

  // create an interpreter  of the monadic algebra
  def runConsole[A](c: Console[A], input: Vector[String], output: Vector[String]): Vector[String] =
    c match {
      case More(Read(r)) =>
        val s = input.head
        runConsole(r(s), input.tail, output)
      case More(Write(v, a)) =>
        runConsole(a, input, output :+ v)
      case Done(_) => output
    }
}
