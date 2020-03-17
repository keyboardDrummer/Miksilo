package miksilo.modularLanguages.deltas.yaml

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser.Result
import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarSequenceCombinatorsExtension, BiGrammarToParser}
import miksilo.modularLanguages.core.bigrammar.grammars.{BiSequence, CustomGrammar}
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

class CheckIndentationGrammar(deltaPredicate: Int => Boolean, property: String, inner: BiGrammar) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Parser[Result]) = {
    BiGrammarToParser.CheckIndentation(deltaPredicate, property, recursive(inner))
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new WithIndentationGrammar(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean) = true
}

object CheckIndentationGrammar {

  def aligned[Element, Sum](grammars: LanguageGrammars, element: BiGrammar): BiGrammar = {
    import grammars._
    val many = equal(element).manyVertical
    new WithIndentationGrammar(BiGrammarSequenceCombinatorsExtension.someMap(
      new BiSequence(element, many, BiSequence.identity, false)))
  }

  def equal[Result](inner: BiGrammar) = new CheckIndentationGrammar(delta => delta == 0, "equal to", inner)
  def greaterThanOrEqualTo[Result](inner: BiGrammar) = new CheckIndentationGrammar(delta => {
    delta >= 0
  }, "greater than or equal to", inner)
  def greaterThan[Result](inner: BiGrammar) = new CheckIndentationGrammar(delta => {
    delta > 0
  }, "greater than", inner)
}