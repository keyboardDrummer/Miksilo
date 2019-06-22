package core.bigrammar.printer

import java.util.Objects

import core.bigrammar.BiGrammar.State
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.{NodePrinter, TryState}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.Success

class ValuePrinter(value: Any) extends NodePrinter {
  override def write(from: WithMap[Any], state: State): TryState[ResponsiveDocument] = {
    if (Objects.equals(value, from.value)) Success(state, Empty)
    else Printer.fail(ValueDiffers(from.value, value), -100)
  }

  case class ValueDiffers(actual: Any, expected: Any) {
    override def toString = s"given value $actual was not equal to value grammar's $expected"
  }
}
