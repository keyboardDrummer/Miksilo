package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.{Result, _}
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

class WithDefault(inner: BiGrammar, _default: Any) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = toDocumentInner(inner) ~ "withDefault: " ~ _default.toString

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = recursive(inner)

  override def toParser(recursive: BiGrammar => Self[Result]): Self[Result] =
    recursive(inner).withDefault[Result](valueToResult(_default))

  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = new core.bigrammar.grammars.WithDefault(newChildren.head, _default)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}

class BiFallback(value: Any, name: String) extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  override def getParser(keywords: collection.Set[String]) = Fallback(value, name)

  override def containsParser(recursive: BiGrammar => Boolean) = false

  override def write(from: WithMap[Any]) = Printer.fail("fallback cannot print")
}
