package free

import cats.free.Free
import org.scalatest.FunSpec

class ParserTest extends FunSpec {
  describe("Free Monad") {
    it("should interpret the chartoy grammar and evaluate it") {
      import CharToyAlgebra._
      import cats._
      import cats.implicits._

      def program(c: Char): Free[CharToyAlgebra, Unit] =
        for {
          _    <- bell
          char <- pure(c)
          _    <- output(char)
          _    <- done
        } yield ()

      val input = 'A'
      assert(showProgram(pure(input)) == s"return ${Show[Char].show(input)}\n")
      assert(showProgram(program(input)) == s"bell\noutput: $input\ndone\n")
    }
  }
}
