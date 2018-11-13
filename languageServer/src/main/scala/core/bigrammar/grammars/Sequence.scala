package core.bigrammar.grammars

import core.bigrammar.BiGrammar

trait Sequence extends BiGrammar with Layout {
  def first: BiGrammar
  def first_=(value: BiGrammar): Unit

  def second: BiGrammar
  def second_=(value: BiGrammar): Unit

  def combine(firstValue: Any, secondValue: Any): Any

  val skip = (a: Any, b: Any) => b
  val const = (a: Any, b: Any) => a
  def const(a: Any, b: Any): Any = a
  def ignoreLeft: Sequence = {
    withCombine(skip)
  }

  def ignoreRight: Sequence = withCombine(const)

  def withCombine(combine: (Any, Any) => Any): Sequence

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
