package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.printer.UndefinedDestructuringValue

object Sequence {

  def packTuple: (Any, Any) => (Any, Any) = (a: Any, b: Any) => (a,b)
  def unpackTuple: Any => (Any, Any) = {
    case UndefinedDestructuringValue => (UndefinedDestructuringValue, UndefinedDestructuringValue)
    case t: (Any, Any) => t
  }
  def ignoreLeft: (Any, Any) => Any = (a: Any, b: Any) => b
  def ignoreRight: (Any, Any) => Any = (a: Any, b: Any) => a
  def produceRight: Any => (Any, Any) = x => (x, UndefinedDestructuringValue)
  def produceLeft: Any => (Any, Any) = x => (x, UndefinedDestructuringValue)
}

trait Sequence extends BiGrammar with Layout {
  def first: BiGrammar
  def first_=(value: BiGrammar): Unit

  def second: BiGrammar
  def second_=(value: BiGrammar): Unit

  def combine: (Any, Any) => Any
  def split: Any => (Any, Any)

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
