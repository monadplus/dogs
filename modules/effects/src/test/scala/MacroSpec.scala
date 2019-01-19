import macros.Macros
import org.scalatest.FunSpec

class MacroSpec extends FunSpec {
  describe("printf") {
    it("should format with %s") {
      val age: Int = 24
      Macros.printf("My is age: %d", age) // prints to the console
      //assert( === s"My age is: $age")
    }
  }
}
