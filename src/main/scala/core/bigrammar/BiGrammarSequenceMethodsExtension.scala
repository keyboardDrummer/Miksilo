package core.bigrammar

import core.bigrammar.grammars._
import core.language.node.Node
import core.document.BlankLine

trait BiGrammarSequenceMethodsExtension extends BiGrammarWriter {

  def grammar: BiGrammar
  def %(bottom: BiGrammar): Sequence
  def ~(other: BiGrammar): Sequence
  def many: ManyHorizontal
  def manyVertical: ManyVertical

  implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceMethodsExtension

  def ~<(right: BiGrammar): BiGrammar = (this ~ right).ignoreRight

  def ~~<(right: BiGrammar): BiGrammar = this ~< new LeftRight(space, right)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

  def ~~(right: BiGrammar): BiGrammar = {
    new IgnoreRight(new LeftRight(grammar, space)) ~ right
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

  def ~>(right: BiGrammar): BiGrammar = (this ~ right).ignoreLeft

  def ~~>(right: BiGrammar): BiGrammar = new LeftRight(grammar, space) ~> right

  def * : ManyHorizontal = many

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }

  def %>(bottom: BiGrammar): BiGrammar = (this % bottom).ignoreLeft

  def %<(bottom: BiGrammar): BiGrammar = (this % bottom).ignoreRight
}
