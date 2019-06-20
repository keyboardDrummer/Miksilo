package deltas.yaml

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.grammars.CustomGrammar
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarSequenceCombinatorsExtension, BiGrammarToParser}
import core.deltas.grammars.LanguageGrammars
import core.responsiveDocument.ResponsiveDocument

class CheckIndentationGrammar(deltaPredicate: Int => Boolean, property: String, inner: BiGrammar) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Self[Result]) = {
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
    new WithIndentationGrammar(BiGrammarSequenceCombinatorsExtension.someMap(element % many))
  }

  def equal[Result](inner: BiGrammar) = new CheckIndentationGrammar(delta => delta == 0, "equal to", inner)
  def greaterThan[Result](inner: BiGrammar) = new CheckIndentationGrammar(delta => delta > 0, "greater than", inner)
}