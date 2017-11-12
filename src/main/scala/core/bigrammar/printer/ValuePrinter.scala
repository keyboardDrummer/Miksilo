package core.bigrammar.printer

import java.util.Objects

import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{NodePrinter, State, fail}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Success, Try}

class ValuePrinter(value: Any) extends NodePrinter {
  override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {

    if (Objects.equals(value, from.value)) Success(state, Empty)
    else fail(ValueDiffers(from.value, value), -100)
  }

  case class ValueDiffers(actual: Any, expected: Any) {
    override def toString = s"given value $actual was not equal to value grammar's $expected"
  }
}
