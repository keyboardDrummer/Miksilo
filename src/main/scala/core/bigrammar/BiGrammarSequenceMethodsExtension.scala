package core.bigrammar

import core.bigrammar.grammars._
import core.deltas.node.Node
import core.document.BlankLine
import core.grammar.~

trait BiGrammarSequenceMethodsExtension extends BiGrammarWriter {

  def grammar: BiGrammar
  def %(bottom: BiGrammar): Sequence
  def ~(other: BiGrammar): Sequence
  def many: ManyHorizontal
  def manyVertical: ManyVertical

  implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceMethodsExtension

  def ~<(right: BiGrammar) = (this ~ right).ignoreRight

  def ~~<(right: BiGrammar) = this ~< new LeftRight(space, right)

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
    grammar ^^
      ( {
        case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
      }, {
        case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
      })
  }
  def inParenthesis = ("(": BiGrammar) ~> grammar ~< ")"

  def ~>(right: BiGrammar): BiGrammar = (this ~ right).ignoreLeft

  def ~~>(right: BiGrammar) = new LeftRight(grammar, space) ~> right

  def * = many

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }

  def %>(bottom: BiGrammar) = (this % bottom).ignoreLeft

  def %<(bottom: BiGrammar) = (this % bottom).ignoreRight
}
