package free

import org.scalatest.FunSpec

class InterpreterTest extends FunSpec {
  describe("Interpreting a KVS algebra using Free monad") {
    import Interpreter._

    it("should modify a KVS element") {
      val res = runKVS(modify("Runar", _ => "is cool!"), Map("Runar" -> "is not cool!"))
      assert(res === Map("Runar" -> "is cool!"))
    }
    it("should get a value and delete it if it does meet a condition") {
      def program(k: String): KVS[Unit] =
        for {
          v <- get(k)
          _ <- if (v.toInt >= 10) delete(k) else put(k, (v.toInt + 1).toString)
        } yield ()

      assert(runKVS(program("count"), Map("count" -> "0")) === Map("count" -> "1"))
      assert(runKVS(program("count"), Map("count" -> "10")) === Map.empty)
    }
  }
}

class Interpreter0Test extends FunSpec {
  describe("Interpreting a Console algebra using Free monad") {
    import Interpreter0._

    it("should read from console and write in memory") {
      val sayHello: Console[Unit] =
        for {
          _    <- putStr("What's your name ?")
          name <- getStr
          _    <- putStr(s"Nice to meet you $name")
        } yield ()

      assert(
        runConsole(sayHello, Vector("Arnau"), Vector.empty) === Vector("What's your name ?",
                                                                       "Nice to meet you Arnau")
      )
    }
  }
}
