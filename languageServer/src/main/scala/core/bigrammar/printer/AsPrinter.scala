package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.Printer.NodePrinter
import core.language.node.NodeField
import core.responsiveDocument.ResponsiveDocument

class AsPrinter(inner: NodePrinter, key: NodeField) extends NodePrinter {
  override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = {
    from.map.get(key).fold[TryState[ResponsiveDocument]](
      Printer.fail(s"did not find as key $key in state ${from.map}"))(
      value => inner.write(WithMapG(value, from.map)))
  }
}
