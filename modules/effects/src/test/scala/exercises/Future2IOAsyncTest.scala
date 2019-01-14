package exercises

import org.scalatest.FunSpec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Future2IOAsyncTest extends FunSpec {
  describe("Future2IOAsyncTest") {

    import scala.concurrent.ExecutionContext.Implicits.global

    it("should convert from finished future to IO") {
      val future = Future(1 + 1)
      Await.result(future, Duration.Inf)
      val io = Future2IOAsync.convert(future)
      assert(io.unsafeRunSync() === 2)
    }

    it("should convert from uncompleted future to async IO") {
      val io = Future2IOAsync.convert(Future(1 + 1))
      assert(io.unsafeRunSync() === 2)
    }
  }
}
