package core.bigrammar.printer

import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

class ManyPrinter[Value](val inner: Printer[Value],
                  val combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument)
  extends Printer[List[Value]] {
  val bindableInner = (value: Value) => inner.write(value).map(left => (right: ResponsiveDocument) => combine(left, right))

  override def write(from: List[Value]): TryState[ResponsiveDocument] = {
    val result: TryState[ResponsiveDocument] = from match {
      case head :: tail =>  new BindPrinter(bindableInner, this).
        write(head, tail) //TODO matching on both list and ArrayBuffer like this is a bit ugly.
      case Nil => TryState.value(Empty)
    }
    result
  }
}
