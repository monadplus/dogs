import org.scalatest.FunSpec
import tailrec.TailRecM._

class TailRecMTest extends FunSpec {

//  def showRes(w: Writer[Log, Long]): Unit = {
//    val res = w.run
//    println(res._1.mkString("\n"))
//    println(s"Result: ${res._2}")
//  }

  describe("not tail recursive") {
    it("should work for small iterations") {
      assert(powWriter(2L, 5L).run._1 === LongProduct(32L))
    }

    it("should blow the stack") {
      try {
        powWriter(1L, 100000L).run._1 === LongProduct(1L)
        fail
      } catch {
        case _: Throwable => succeed
      }
    }
  }
  describe("tail recursive") {
    it("should work small iterations") {
      assert(powWriter2(2L, 5L).run._1 === LongProduct(32L))
    }
    it("should not blow the stack") {
      try {
        assert(powWriter2(1L, 100000L).run._1 === LongProduct(1L))
      } catch {
        case _: Throwable => fail
      }
    }
  }

  describe("DIY tail recursive writer") {
    import tailrec.TailRecMDIY._

    it("should compute small numbers") {
      assert(powWriter4(3L, 5L).run._2 === 243L)
    }
    it("should compute large numbers without blowing the stack") {
      try {
        assert(powWriter4(1L, 100000L).run._2 === 1L)
      } catch {
        case _: Throwable => fail
      }
    }
  }
}
