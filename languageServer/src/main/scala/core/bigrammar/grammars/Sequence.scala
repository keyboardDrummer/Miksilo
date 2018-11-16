package core.bigrammar.grammars

import core.bigrammar.BiGrammar

object Sequence {

  def ignoreLeft: (Any, Any) => Any = (a: Any, b: Any) => b
  def ignoreRight: (Any, Any) => Any = (a: Any, b: Any) => a

}

trait Sequence extends BiGrammar with Layout {
  def first: BiGrammar
  def first_=(value: BiGrammar): Unit

  def second: BiGrammar
  def second_=(value: BiGrammar): Unit

  def combine(firstValue: Any, secondValue: Any): Any

  override def children = Seq(first, second)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean =
    recursive(first) || recursive(second)

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]): Seq[BiGrammar] = {
    if (first.containsParser())
      recursive(first)
    else {
      recursive(second)
    }
  }
}
