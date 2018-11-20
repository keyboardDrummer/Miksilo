package core.bigrammar

import core.bigrammar.grammars._
import core.document.BlankLine
import core.language.node.Node

trait BiGrammarSequenceCombinatorsExtension extends BiGrammarWriter {

  def grammar: BiGrammar
  def sequence(other: BiGrammar, bijective: SequenceBijective, horizontal: Boolean): BiGrammar

  def ~(other: BiGrammar): BiGrammar = sequence(other, Sequence.identity, true)
  def %(other: BiGrammar): BiGrammar = sequence(other, Sequence.identity, false)

  def ~<(right: BiGrammar): BiGrammar = sequence(right, Sequence.ignoreRight, true)

  def ~>(right: BiGrammar): BiGrammar = sequence(right, Sequence.ignoreLeft, true)

  def %>(bottom: BiGrammar): BiGrammar = sequence(bottom, Sequence.ignoreLeft, false)

  def %<(bottom: BiGrammar): BiGrammar = sequence(bottom, Sequence.ignoreRight, false)

  def many: ManyHorizontal
  def manyVertical: ManyVertical

  implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension

  def ~~<(right: BiGrammar): BiGrammar = this ~< new BiSequence(printSpace, right, Sequence.ignoreLeft, true)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

  def ~~(right: BiGrammar): BiGrammar = {
    new BiSequence(grammar, printSpace, Sequence.ignoreRight, true) ~ right
  }

  def someSeparatedVertical(separator: BiGrammar): BiGrammar =
    someMap(this % (separator %> grammar).manyVertical)

  def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | ValueGrammar(Seq.empty[Node])

  def some: BiGrammar = someMap(grammar ~ (grammar*))
  def someSeparated(separator: BiGrammar): BiGrammar = someMap(this ~ ((separator ~> grammar) *))

  private def someMap(grammar: BiGrammar): BiGrammar = {
    grammar.mapSome[(Any, Seq[Any]), Seq[Any]](
      t => Seq(t._1) ++ t._2,
      seq => if (seq.nonEmpty) Some((seq.head, seq.tail)) else None)
  }

  def inParenthesis: BiGrammar = ("(": BiGrammar) ~> grammar ~< ")"

  def ~~>(right: BiGrammar): BiGrammar = new BiSequence(grammar, printSpace, Sequence.ignoreRight, true) ~> right

  def * : ManyHorizontal = many

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }
}

