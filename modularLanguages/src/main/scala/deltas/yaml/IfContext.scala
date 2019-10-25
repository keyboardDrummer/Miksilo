package deltas.yaml

import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.bigrammar.grammars.CustomGrammar
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument
import deltas.yaml.YamlCoreDelta.IfContextParser

object IfContext {
  val contexts = Seq(BlockOut, BlockIn, BlockKey, FlowKey, FlowOut, FlowIn)
}

class IfContext[Result](inners: Map[YamlContext, BiGrammar], printer: BiGrammar) extends CustomGrammar with BiGrammar {
  import IfContext._

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(printer)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(printer)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Parser[BiGrammarToParser.Result]) =
    new IfContextParser(inners.mapValues(recursive))

  override def children = contexts.flatMap(inners.get(_).toSeq)

  override def withChildren(newChildren: Seq[BiGrammar]) =
    new IfContext(contexts.filter(c => inners.contains(c)).zip(newChildren).toMap, newChildren.last)

  override def containsParser(recursive: BiGrammar => Boolean) = true
}
