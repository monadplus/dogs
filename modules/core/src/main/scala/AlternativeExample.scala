/**
  * Alternative extends Applicative with a MonoidK
  */
object AlternativeExample extends App {
  /*
    trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] {
      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

      def pure[A](a: A): F[A]

      def empty[A]: F[A]

      def combineK[A](x: F[A], y: F[A]): F[A]
  }
   */

  import cats._
  import cats.implicits._

  // Vector example

  Alternative[Vector].empty[Int]
  val double: Int => Int = _ * 2
  val addFive: Int => Int = _ + 5
  val fvab: Vector[Int => Int] = double.pure[Vector] <+> addFive.pure[Vector] // Vector(double, addFive)
  fvab ap (7.pure[Vector] <+> 8.pure[Vector]) // Vector(7*2, 7+5, 8*2, 8+5)

  // Alternative parser

  trait Decoder[A] {
    def decode(in: String): Either[Throwable, A]
  }
  object Decoder {
    def from[A](f: String => Either[Throwable, A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = f(in)
      }
  }

  implicit val decoderAlternative = new Alternative[Decoder] {
    def pure[A](a: A) = Decoder.from(Function.const(Right(a)))

    def empty[A] = Decoder.from(Function.const(Left(new Error("No dice."))))

    def combineK[A](l: Decoder[A], r: Decoder[A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = l.decode(in).orElse(r.decode(in))
      }

    def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
      new Decoder[B] {
        def decode(in: String) = fa.decode(in) ap ff.decode(in)
      }
  }

  def parseInt(s: String): Either[Throwable, Int] = Either.catchNonFatal(s.toInt)
  def parseIntFirstChar(s: String): Either[Throwable, Int] = Either.catchNonFatal(Character.digit(s.charAt(0), 10))

  val decoder: Decoder[Int] = Decoder.from(parseInt) <+> Decoder.from(parseIntFirstChar)

  println(s"Decoding 555: ${decoder.decode("555")}") // 555
  println(s"Decoding 5a: ${decoder.decode("5a")}") // 5

  // Partitioning results

  /*
  def separate[G[_, _], A, B](fgab: F[G[A, B]])(implicit FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) = {
    val as = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(pure, _ => empty[A])(algebra[A]))
    val bs = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(_ => empty[B], pure)(algebra[B]))
    (as, bs)
  }
   */

  def requestResource(a: Int): Either[(Int, String), (Int, Long)] = {
    if (a % 4 == 0) Left((a, "Bad request"))
    else if (a % 3 == 0) Left((a, "Server error"))
    else Right((a, 200L))
  }

  val partitionedResults: (Vector[(Int, String)], Vector[(Int, Long)]) =
    ((requestResource _).pure[Vector] ap Vector(5, 6, 7, 99, 1200, 8, 22)).separate

  println{
    s"""Partitioned results:
       | - Failed requests: ${partitionedResults._1}
       | - Succeeded requests: ${partitionedResults._2}""".stripMargin
  }

  // -------

  def getRegionAndDistrict(pkey: Int): (Int, Vector[Int]) = (pkey, (double.pure[Vector] <+> addFive.pure[Vector]) ap pkey.pure[Vector])

  val regionsWithDistricts = (getRegionAndDistrict _).pure[Vector] ap Vector(5, 6, 7, 97, 1200, 8, 25)

  val regionIds: Vector[Int] = regionsWithDistricts.separate._1
  val districtIds: Vector[Int] = regionsWithDistricts.separate._2.flatten

  println(s"* Region IDs: $regionIds\n* Districts IDs: $districtIds")
}
