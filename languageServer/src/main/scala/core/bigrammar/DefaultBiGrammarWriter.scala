package core.bigrammar

import core.bigrammar.grammars._

import scala.reflect.ClassTag

object DefaultBiGrammarWriter extends DefaultBiGrammarWriter

trait DefaultBiGrammarWriter extends AbstractGrammarWriter[BiGrammar] {
//
//  implicit def stringToAstGrammar(value: String): BiGrammarExtension =
//    new BiGrammarExtension(BiGrammarWriter.stringToGrammar(value))
//
//  implicit def grammarToAstGrammar(value: BiGrammar): BiGrammarExtension = new BiGrammarExtension(value)
  override def map2[Value, NewValue: ClassTag](grammar: BiGrammar[Value],
                                               afterParsing: Value => NewValue,
                                               beforePrinting: NewValue => Option[Value]) = {
    new MapGrammar[Value, NewValue](grammar, value => Right(afterParsing(value)), beforePrinting)
  }

  override def choice2[Value](grammar: BiGrammar[Value], other: BiGrammar[Value], firstIsLonger: Boolean) = {
    new BiChoice(grammar, other, firstIsLonger)
  }

  override def sequence2[Value, Other, Result](grammar: BiGrammar[Value], other: BiGrammar[Other],
                                               bijective: SequenceBijective[Value, Other, Result],
                                               horizontal: Boolean) = {
    new BiSequence(grammar, other, bijective, horizontal)
  }

  override def many2[Value](grammar: BiGrammar[Value], horizontal: Boolean) =
    if (horizontal) new ManyHorizontal[Value](grammar) else new ManyVertical[Value](grammar)

  override implicit def stringToGrammar(value: String): BiGrammar[Unit] = ???

  override implicit def wrapBiGrammar[T](grammar: BiGrammar[T]): BiGrammar[T] = ???
}
