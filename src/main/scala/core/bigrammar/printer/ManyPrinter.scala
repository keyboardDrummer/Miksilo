package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.Printer.NodePrinter
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

class ManyPrinter(val inner: NodePrinter,
                  val combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument)
  extends NodePrinter {
  val bindableInner = (value: WithMapG[Any]) => inner.write(value).map(left => (right: ResponsiveDocument) => combine(left, right))

  override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = {
    val result: TryState[ResponsiveDocument] = from.value match {
      case seq: Seq[_] if seq.nonEmpty => new BindPrinter(bindableInner, this).
        write(WithMapG(core.grammar.~(seq.head, seq.tail), from.map)) //TODO matching on both list and ArrayBuffer like this is a bit ugly.
      case _: Seq[_] => TryState.value(Empty)
      case UndefinedDestructuringValue => TryState.value(Empty)
      case _ => Printer.fail(s"$from passed to many was not a list")
    }
    result
  }
}
