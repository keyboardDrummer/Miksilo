package core.bigrammar.printer

import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

class ManyPrinter(val inner: NodePrinter,
                  val combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument)
  extends NodePrinter {
  val bindableInner = (value: WithMap[Any]) => inner.write(value).map(left => (right: ResponsiveDocument) => combine(left, right))

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
    val result: TryState[ResponsiveDocument] = from.value match {
      case seq: Seq[_] if seq.nonEmpty => new BindPrinter(bindableInner, this).
        write(WithMap((seq.head, seq.tail), from.namedValues)) //TODO matching on both list and ArrayBuffer like this is a bit ugly.
      case _: Seq[_] => TryState.value(Empty)
      case UndefinedDestructuringValue => TryState.value(Empty)
      case _ => Printer.fail(s"$from passed to many was not a list")
    }
    result
  }
}
