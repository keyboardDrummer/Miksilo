package core.bigrammar.printer

import java.util.Objects

import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

class ValuePrinter(value: Any) extends NodePrinter {
  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
    if (Objects.equals(value, from.value)) TryState.value(Empty)
    else Printer.fail(ValueDiffers(from.value, value), -100)
  }

  case class ValueDiffers(actual: Any, expected: Any) {
    override def toString = s"given value $actual was not equal to value grammar's $expected"
  }
}
