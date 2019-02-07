package mononocle.typeclasses

import monocle.{Optional, Traversal}
import monocle.function.{At, Index}
import monocle.macros.GenLens

/**
 * Typeclass that defines an [[Optional]] from an `S` to an `A` at an index `I`
 * [[Index]] is less powerful than [[At]] as it cannot create or delete value
 *
 * @tparam S source of [[Optional]]
 * @tparam I index
 * @tparam A target of [[Optional]], `A` is supposed to be unique for a given pair `(S, I)`
 */
object indexExample extends App {
  case class Key(opens: String)
  case class KeyChainStore(keys: List[Key])
  val keyChainStore = KeyChainStore(List(Key("door 1"), Key("door 2"), Key("door 3")))

  import monocle.function.Index._

  val keys                                            = GenLens[KeyChainStore](_.keys)
  val keyChainStore4Key: Optional[KeyChainStore, Key] = keys.composeOptional(index(4))
  val keyChainStore2                                  = keyChainStore4Key.set(Key("door 4"))(keyChainStore)
  // Set does nothing as it is an optional field
  println(s"Keychain store 2: $keyChainStore2")

  import monocle.function.FilterIndex._

  /**
   *  [[ FilterIndex ]]
   *
   * Typeclass that defines a [[Traversal]] from an `S` to all its elements `A` whose index `I` in `S` satisfies the predicate
   * @tparam S source of [[Traversal]]
   * @tparam I index
   * @tparam A target of [[Traversal]], `A` is supposed to be unique for a given pair `(S, I)`
   */
  val evenKeys: Traversal[KeyChainStore, Key] = keys.composeTraversal(listFilterIndex.filterIndex(_ % 2 == 0))
  val keyChainStore3                          = evenKeys.set(Key("even"))(keyChainStore2)
  println(s"Keychain store 3: $keyChainStore3")
}
