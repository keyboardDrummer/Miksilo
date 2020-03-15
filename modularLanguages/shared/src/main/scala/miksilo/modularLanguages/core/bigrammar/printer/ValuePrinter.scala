package miksilo.modularLanguages.core.bigrammar.printer

import java.util.Objects

import miksilo.modularLanguages.core.bigrammar.WithMap
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.document.Empty
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

class ValuePrinter(value: Any) extends NodePrinter {
  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
    if (Objects.equals(value, from.value)) TryState.value(Empty)
    else Printer.fail(ValueDiffers(from.value, value), -100)
  }

  case class ValueDiffers(actual: Any, expected: Any) {
    override def toString = s"given value $actual was not equal to value grammar's $expected"
  }
}
