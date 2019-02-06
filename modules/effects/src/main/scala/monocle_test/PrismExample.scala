package monocle_test

import java.io.Serializable

/**
 * A Prism is an optic used to select part of a Sum type
 *
 * Prisms have two type parameters generally called S and A: Prism[S, A]
 *    where S represents the Sum and A a part of the Sum.
 */
sealed trait Json                     extends Serializable
case object JNull                     extends Json
case class JStr(v: String)            extends Json
case class JNum(v: Double)            extends Json
case class JObj(v: Map[String, Json]) extends Json

object PrismExample {
  import monocle.Prism

  val jStr = Prism[Json, String] {
    case JStr(v) => Some(v)
    case _       => None
  }(JStr)

  val jStr2 = Prism.partial[Json, String] { case JStr(v) => v }(JStr)

  jStr("hello")                 //   def apply(b: B): T = reverseGet(b)
  jStr.getOption(JStr("Hello")) // Some
  jStr.getOption(JNum(1.0))     // None

  // Used in a patter matching position:

  def isLongString(json: Json): Boolean = json match {
    case jStr(s) => s.length > 100
    case _       => false
  }

  jStr.set("Bar")(JStr("Hello"): Json)
  jStr.modify(_.reverse)(JStr("olleH"): Json) // no-op if not a JStr
  jStr.modifyOption(_.reverse)(JNum(1.0))

  // Composability

  import monocle.std.double.doubleToInt // Prism[Double, Int] defined in Monocle

  val jNum: Prism[Json, Double] =
    Prism.partial[Json, Double] { case JNum(v) => v }(JNum)

  val jInt: Prism[Json, Int] =
    jNum.composePrism(doubleToInt)

  jInt.reverseGet(1)
  jInt.getOption(JNum(1.0))

  import monocle.macros.GenPrism
  import monocle.macros.GenIso
  import monocle.macros.GenLens

  val rawJNum: Prism[Json, JNum] = GenPrism[Json, JNum]
  val jInt2                      = GenPrism[Json, JNum].composeIso(GenIso[JNum, Double])
  val jInt3                      = GenPrism[Json, JNum].composeLens(GenLens[JNum](_.v))

  // Laws

  def partialRoundTripOneWay[S, A](p: Prism[S, A], s: S): Boolean =
    p.getOption(s) match {
      case None    => true // nothing to prove
      case Some(a) => p.reverseGet(a) == s
    }

  def partialRoundTripAnotherWay[S, A](p: Prism[S, A], a: A): Boolean =
    p.getOption(p.reverseGet(a)) == Some(a)
}
