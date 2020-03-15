package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.modularLanguages.core.bigrammar.WithMap
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.modularLanguages.core.node.NodeField
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

class AsPrinter(inner: NodePrinter, key: NodeField) extends NodePrinter {
  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
    from.namedValues.get(key).fold[TryState[ResponsiveDocument]](
      Printer.fail(s"did not find as key $key in state ${from.namedValues}"))(
      value => inner.write(WithMap(value, from.namedValues)))
  }
}
