package deltas.yaml

import core.bigrammar.BiGrammarToParser.Rec
import core.bigrammar.grammars.{BiSequence, CustomGrammar}
import core.bigrammar.printer.BiGrammarToPrinter.ToPrinterCached
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.deltas.grammars.LanguageGrammars
import core.responsiveDocument.ResponsiveDocument

class CheckIndentationGrammar[Value](deltaPredicate: Int => Boolean, property: String, inner: BiGrammar[Value]) extends CustomGrammar[Value] {
  override def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: ToPrinterCached) = recursive(inner)

  override def toParser(recursive: Rec) = {
    BiGrammarToParser.CheckIndentation(deltaPredicate, property, recursive(inner))
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    new CheckIndentationGrammar(deltaPredicate, property, newChildren.head.asInstanceOf[BiGrammar[Value]])

  override def containsParser(recursive: BiGrammar[_] => Boolean) = true
}

object CheckIndentationGrammar {

  def aligned[Element](grammars: LanguageGrammars, element: BiGrammar[WithMap[Element]]): BiGrammar[WithMap[List[Element]]] = {
    import grammars._

    val many = equal(element).manyVertical
    new WithIndentationGrammar(new BiSequence(element, many, withMapBijective(listConsBijective), false))
  }

  def equal[Value](inner: BiGrammar[Value]) = new CheckIndentationGrammar(delta => delta == 0, "equal to", inner)
  def greaterThan[Value](inner: BiGrammar[Value]) = new CheckIndentationGrammar(delta => {
    delta > 0
  }, "greater than", inner)
}