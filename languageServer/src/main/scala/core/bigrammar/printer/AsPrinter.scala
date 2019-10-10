package core.bigrammar.printer

import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.language.node.NodeField
import core.responsiveDocument.ResponsiveDocument

class AsPrinter[Value](inner: Printer[Value], key: NodeField) extends Printer[WithMap[Unit]] {
  override def write(from: WithMap[Unit]): TryState[ResponsiveDocument] = {
    from.namedValues.get(key).fold[TryState[ResponsiveDocument]](
      Printer.fail(s"did not find as key $key in state ${from.namedValues}"))(
      value => inner.write(value.asInstanceOf[Value]))
  }
}
