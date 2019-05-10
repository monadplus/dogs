import org.scalatest.FunSpec

import java.io._

import cats.effect.IO
import cats.effect._
import cats.effect.internals.IOContextShift

class ConcurrencySpec extends FunSpec {

  // TODO: Concurrent requires ContextShift
  implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  def readFileContent(file: File): String = {
    val reader = new BufferedReader(new FileReader(file))
    val content = new StringBuilder()
    var line: String = null
    var firstLine = true
    do {
      if (!firstLine && line != null) content.append("\n")
      else firstLine = false
      line = reader.readLine()
      if (line != null)
        content.append(line)
    } while (line != null)
    content.mkString
  }

  describe("CopyFile") {

    import concurrency.CopyFiles2._

    val text =
      """
        |JULIET appears above at a window
        |But, soft! what light through yonder window breaks?
        |It is the east, and Juliet is the sun.
        |Arise, fair sun, and kill the envious moon,
        |Who is already sick and pale with grief,
        |That thou her maid art far more fair than she:
      """.stripMargin
    val input = File.createTempFile("input", ".txt")
    val output = File.createTempFile("output", ".txt")
    input.deleteOnExit()
    output.deleteOnExit()

    val writer = new FileOutputStream(input)
    writer.write(text.getBytes())

    it("should transfer the content of a file to another file") {
      val copiedBytes = copy[IO](input, output).unsafeRunSync()
      assert(copiedBytes === text.getBytes().length)
      val res = readFileContent(output)
      assert(res.trim() === text.trim())
    }
  }
}
