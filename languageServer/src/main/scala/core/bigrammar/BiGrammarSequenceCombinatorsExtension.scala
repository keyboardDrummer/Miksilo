package core.bigrammar

import core.bigrammar.grammars._
import core.document.BlankLine
import core.language.node.Node

trait BiGrammarSequenceCombinatorsExtension extends BiGrammarWriter {

  def grammar: BiGrammar
  def topBottom(other: BiGrammar, bijective: SequenceBijective): TopBottom
  def leftRight(other: BiGrammar, bijective: SequenceBijective): LeftRight

  def ~(other: BiGrammar): LeftRight = leftRight(other, Sequence.identity)
  def %(other: BiGrammar): TopBottom = topBottom(other, Sequence.identity)

  def ~<(right: BiGrammar): BiGrammar = leftRight(right, Sequence.ignoreRight)

  def ~>(right: BiGrammar): BiGrammar = leftRight(right, Sequence.ignoreLeft)

  def %>(bottom: BiGrammar): BiGrammar = topBottom(bottom, Sequence.ignoreLeft)

  def %<(bottom: BiGrammar): BiGrammar = topBottom(bottom, Sequence.ignoreRight)

  def many: ManyHorizontal
  def manyVertical: ManyVertical

  implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension

  def ~~<(right: BiGrammar): BiGrammar = this ~< new LeftRight(printSpace, right, Sequence.ignoreLeft)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

  def ~~(right: BiGrammar): BiGrammar = {
    new LeftRight(grammar, printSpace, Sequence.ignoreRight) ~ right
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

  def ~~>(right: BiGrammar): BiGrammar = new LeftRight(grammar, printSpace, Sequence.ignoreRight) ~> right

  def * : ManyHorizontal = many

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }
}

