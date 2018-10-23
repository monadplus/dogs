package monad

import org.scalatest.FunSpec

class TailRecMTest extends FunSpec {
  import TailRecM._

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
        powWriter2(1L, 100000L).run._1 === LongProduct(1L)
        succeed
      } catch {
        case _: Throwable => fail
      }
    }
  }
}
