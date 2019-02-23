package data

import cats.arrow.FunctionK
import cats.{Show, ~>}
import cats.effect.IO

object FreeMonadExample extends App {

  /**
    * In particular, free monads provide a practical way to:
    *
    * - represent stateful computations as data, and run them
    * - run recursive computations in a stack-safe way
    * - build an embedded DSL (domain-specific language)
    * - retarget a computation to another interpreter using natural transformations
    *
    */
  // Free[_] is a programming language inside your programming language!

  /* Steps:

    1.- Create an Algebra[_]
    2.- Create a type based on Free[_]
    3.- Create smart constructors for your algebra using liftF (GADT using smart constructors).
    4.- Build a program
    5.- Build a compiler
    6.- Execute our compiled program.
   */

  // 1.-
  sealed trait Algebra[A]
  final case class Put[T](key: String, value: T) extends Algebra[Unit]
  final case class Get[T](key: String) extends Algebra[Option[T]]
  final case class Delete(key: String) extends Algebra[Unit]

  // 2.-
  import cats.free.Free
  type KVStore[A] = Free[Algebra, A]

  // 3.-
  import cats.free.Free.liftF

  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[Algebra, Unit](Put(key, value))

  def get[T](key: String): KVStore[Option[T]] =
    liftF[Algebra, Option[T]](Get(key))

  def delete(key: String): KVStore[Unit] =
    liftF[Algebra, Unit](Delete(key))

  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      optT <- get[T](key)
      _ <- optT.map(t => put[T](key, f(t))).getOrElse(Free.pure(()))
    } yield ()

  // 4.-
  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // 5.-
  import cats.~>

//  def impureCompiler: Algebra ~> Id = new (Algebra ~> Id) {
//    val kvs = mutable.Map.empty[String, Any]
//
//    def apply[A](fa: Algebra[A]): Id[A] =
//      fa match {
//        case Put(key, value) =>
//          kvs(key) = value
//          ().asInstanceOf[Id[A]]
//        case Get(key) =>
//          kvs.get(key).asInstanceOf[Id[A]]
//        case Delete(key) =>
//          kvs.remove(key)
//          ().asInstanceOf[Id[A]]
//      }
//  }

  import cats.data._
  type KVStoreState[A] = State[Map[String, Any], A]

  val compiler = λ[Algebra ~> KVStoreState] {
    case Put(k, v) =>
      State.modify[Map[String, Any]](_ + (k -> v))
    case Get(k) =>
      State.inspect[Map[String, Any], Option[Any]](_.get(k))
    case Delete(k) =>
      State.modify[Map[String, Any]](_ - k)
  }

  // 6.- Stack-safety

  val (map, res) = program.foldMap[KVStoreState](compiler).run(Map.empty).value
  println(s"Program: ($map, $res)")
  // -----------------------------
  // -----------------------------
  // -----------------------------
  // -----------------------------
  // -----------------------------

  // Composing Free Monads

  import cats.data.EitherK
  import cats.free.Free
  import cats.{Id, InjectK, ~>}
  import scala.collection.mutable.ListBuffer

  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  sealed trait DataOp[A]
  case class AddCat(a: String) extends DataOp[Unit]
  case class GetAllCats() extends DataOp[List[String]]

  // Composition of Algebras
  type CatsApp[A] = EitherK[DataOp, Interact, A]

  // Smart constructors
  class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
    def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
    def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
  }
  object Interacts {
    implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
  }
  class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
    def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
    def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
  }
  object DataSource {
    implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
  }

  // Program
  def catsProgram(implicit I: Interacts[CatsApp], D: DataSource[CatsApp]): Free[CatsApp, Unit] = {
    import I._, D._
    for {
      cat <- ask("What's the kitty's name?")
      _ <- addCat(cat)
      cats <- getAllCats
      _ <- tell(cats.toString)
    } yield ()
  }

  // Interpreter
  /*_*/
  object ConsoleCatsInterpreter extends (Interact ~> Id) {
    override def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Ask(prompt) =>
        println(prompt)
        scala.io.StdIn.readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

    private[this] val memDataSet = new ListBuffer[String]

    override def apply[A](fa: DataOp[A]): Id[A] = fa match {
      case AddCat(a)    => memDataSet.append(a)
      case GetAllCats() => memDataSet.toList
    }
  }
  /*_*/

  val catsInterpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter.or(ConsoleCatsInterpreter)

  // Execute program
  catsProgram.foldMap(catsInterpreter)
}

object FreeMonadExample2 extends App {

  sealed trait AlgebraA[A]
  final case class Tell[T: Show](t: T) extends AlgebraA[Unit]
  final case object Ask extends AlgebraA[String]
  final case object Done extends AlgebraA[Unit]

  import cats.free.Free
  type Algebra[A] = Free[AlgebraA, A]

  import cats.free.Free.liftF
  import cats.implicits._
  def tell[T: Show](t: T): Algebra[Unit] =
    liftF(Tell[T](t): AlgebraA[Unit])

  def ask: Algebra[String] =
    liftF(Ask)

  val done: Algebra[Unit] =
    liftF(Done)

  def askThenTell: Algebra[Unit] =
    for {
      input <- ask
      _ <- tell(input)
    } yield ()

  def program: Algebra[Unit] =
    for {
      _ <- tell("Introduce your name")
      name <- ask
      _ <- tell("Introduce your surname")
      surname <- ask
      _ <- tell(s"Hello $name $surname")
      _ <- done
    } yield ()

  /*_*/
  object consoleInterpreter extends (AlgebraA ~> IO) {
    override def apply[A](fa: AlgebraA[A]): IO[A] = fa match {
      case Tell(s) => IO(println(s))
      case Ask     => IO(scala.io.StdIn.readLine())
      case Done    => IO.unit
    }
  }
  /*_*/

  program.foldMap[IO](consoleInterpreter).unsafeRunSync()
}
