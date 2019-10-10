package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, WithMap}
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.{Printer, TryState}
import core.responsiveDocument.ResponsiveDocument

import scala.reflect.ClassTag

case class Keyword(var value: String, reserved: Boolean = true)
  extends CustomGrammarWithoutChildren[Unit] {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = true

  override def write(from: Unit): TryState[ResponsiveDocument] = TryState.value(value)

  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[String] = {
    if (reserved)
      literalOrKeyword(value)
    else
      literal(value)
  }

  def setValue[T: ClassTag](newValue: T): BiGrammar[T] = new MapGrammar[Unit, T](this, _ => Right(newValue), {
    aValue => if (aValue == newValue) Some(value) else None
  })
}
