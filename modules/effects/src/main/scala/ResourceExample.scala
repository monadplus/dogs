import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._

object ResourceExample extends IOApp {
  def mkFailingResource(s: String): Resource[IO, String] = {
    val acquire =
      IO(println(s"Acquiring $s")) *> IO.pure(s)

    def release(s: String) =
      IO.raiseError(new Exception(s"Failed releasing $s"))

    Resource.make(acquire)(release)
  }

  def mkResource(s: String): Resource[IO, String] = {
    val acquire =
      IO(println(s"Acquiring $s")) *> IO.pure(s)

    def release(s: String) =
      IO(println(s"Realising $s")) *> IO.pure(())

    Resource.make(acquire)(release)
  }

  val r = for {
    outer <- mkResource("outer")
    inner <- mkFailingResource("inner")
  } yield (outer, inner)

  override def run(args: List[String]): IO[ExitCode] =
    r.use { case (a, b) => IO(println(s"Using $a and $b")) } >> IO.pure(ExitCode.Success)
}
