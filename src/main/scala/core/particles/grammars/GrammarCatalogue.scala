package core.particles.grammars

import java.util.NoSuchElementException

import core.bigrammar._
import core.particles.GrammarWithTrivia
import core.particles.node.{GrammarKey, Key}

case class KeyGrammar(key: Key) extends GrammarKey
{
  override lazy val toString = key.toString
}


object WhitespaceGrammar extends GrammarKey
class GrammarCatalogue {

  var grammars: Map[Any, Labelled] = Map.empty

  implicit def stringToAstGrammar(value: String) = new GrammarWithTrivia(Keyword(value))(this)
  implicit def grammarToAstGrammar(value: BiGrammar) = new GrammarWithTrivia(value)(this)

  val whiteSpace: Labelled = new Labelled(WhitespaceGrammar, ParseWhiteSpace)
  def root: Labelled = find(ProgramGrammar)
  def find(key: GrammarKey): Labelled = {
    try {
      grammars(key)
    } catch {
      case e: NoSuchElementException => throw GrammarNotFoundException(key, e)
    }
  }

  def create(key: GrammarKey, inner: BiGrammar = BiFailure()): Labelled = {
    val result = new Labelled(key, inner)
    grammars += key -> result
    result
  }

  def findPath(to: GrammarKey, from: GrammarKey): GrammarReference = {
    val rootGrammar = new RootGrammar(find(from))
    rootGrammar.findLabelled(to)
  }

//  def indent(grammar: BiGrammar, width: Int = 2) = GrammarForAst(WhiteSpace(width, 0)) ~> grammar
//
//  def ~<(grammar: BiGrammar, right: BiGrammar) = (grammar ~ right).ignoreRight
//
//  def ~~<(grammar: BiGrammar, right: BiGrammar) = grammar ~< (space ~ right)
//
//  def manySeparated(grammar: BiGrammar, separator: BiGrammar): BiGrammar = someSeparated(grammar, separator) | ValueGrammar(Seq.empty[Any])
//
//  def ~~(grammar: BiGrammar, right: BiGrammar): BiGrammar = {
//    (grammar ~< space) ~ right
//  }
//
//  def someSeparatedVertical(grammar: BiGrammar, separator: BiGrammar): BiGrammar =
//    someMap(this % (separator %> grammar).manyVertical)
//
//  def manyVertical(grammar: BiGrammar) = new ManyVertical(WithWhiteSpace(grammar, whiteSpace))
//
//  def manySeparatedVertical(grammar: BiGrammar, separator: BiGrammar): BiGrammar = someSeparatedVertical(grammar, separator) | ValueGrammar(Seq.empty[Node])
//
//  def some(grammar: BiGrammar): BiGrammar = someMap(grammar ~ (grammar*))
//  def someSeparated(grammar: BiGrammar, separator: BiGrammar): BiGrammar = someMap(grammar ~ ((separator ~> grammar) *))
//
//  private def someMap(grammar: BiGrammar): BiGrammar = {
//    grammar ^^
//      ( {
//        case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
//      }, {
//        case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
//      })
//  }
//  def inParenthesis(grammar: BiGrammar) = ("(": BiGrammar) ~> grammar ~< ")"
//
//  def ~(grammar: BiGrammar, other: BiGrammar) = new Sequence(grammar, WithWhiteSpace(other, whiteSpace))
//
//  def ~>(grammar: BiGrammar, right: BiGrammar): BiGrammar = (grammar ~ right).ignoreLeft
//
//  def ~~>(grammar: BiGrammar, right: BiGrammar) = (grammar ~ space) ~> right
//
//  def *(grammar: BiGrammar) = new ManyHorizontal(WithWhiteSpace(grammar, whiteSpace))
//  def many = this*
//
//  def %(grammar: BiGrammar, bottom: BiGrammar) = new TopBottom(grammar, WithWhiteSpace(bottom, whiteSpace))
//
//  def %%(bottom: BiGrammar): BiGrammar = {
//    (this %< BlankLine) % bottom
//  }
//
//  def %>(bottom: BiGrammar) = (this % bottom).ignoreLeft
//
//  def %<(bottom: BiGrammar) = (this % bottom).ignoreRight
}

case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
