package core.bigrammar.grammars

import core.bigrammar.BiGrammar

trait SequenceLike extends BiGrammar with Layout {
  def first: BiGrammar
  def first_=(value: BiGrammar): Unit

  def second: BiGrammar
  def second_=(value: BiGrammar): Unit

  override def children = Seq(first, second)

  def ignoreLeft: MapGrammar = new IgnoreLeft(this)

  def ignoreRight: MapGrammar = new IgnoreRight(this)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean =
    recursive(first) || recursive(second)
}
