import org.scalatest._
import equality.all._
import org.scalatest.exceptions.TestFailedException

class EqualityCoreSpec extends FreeSpec {

  sealed trait XY
  case class X(int: Int) extends XY
  case class Y(bool: Boolean) extends XY
  case class A(a: XY, a2: Long)

  "Equality library" - {
    "should be able to compare arbitrary ADTs" in {
      val a1 = A(X(1), 10L)
      val a2 = A(X(10), 11L)
      val a3 = A(Y(false), 10L)

      a1 ==== a1
      intercept[TestFailedException] {
        a1 ==== a2
      }
      intercept[TestFailedException] {
        a1 ==== a3
      }
    }
  }
}
