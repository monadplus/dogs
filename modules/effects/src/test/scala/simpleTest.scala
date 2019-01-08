import org.scalatest.FunSpec

class simpleTest extends FunSpec {
  trait KVS[A]
  case class Get(k: String)            extends KVS[Option[String]]
  case class Put(k: String, v: String) extends KVS[Unit]

  trait Translator[F[_], G[_]] {
    def apply[B](in: F[B]): G[B]
  }

  //  def translator: Translator[Instr, Task] = new Translator[Instr, Task] {
  //    def apply[B](in: Instr[B]): Task[B] = in match {
  //      case Put(k, v) => Task.delay (DB.put(k, v))
  //      case Get(k) => Task.delay(DB.get(k))
  //    }

//  todo: was working on it
//  def translator: Translator[KVS, IO] = new Translator[KVS, IO] {
//    override def apply[B](in: KVS[B]): IO[B] = in match {
//      case Get(k)    => IO.delay(k)
//      case Put(k, v) => ???
//    }
//  }
}
