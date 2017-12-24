package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

class SequencePrinter(first: NodePrinter, second: NodePrinter,
                      combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument) extends NodePrinter {
  private val newFirst = (value: WithMapG[Any]) => first.write(value).map(
    firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

  val tuplePrinter = new BindPrinter[Any, Any](newFirst, second)

  override def write(value: WithMapG[Any]): TryState[ResponsiveDocument] = value.value match {
    case tuple: (Any, Any) => tuplePrinter.write(WithMapG(tuple, value.map))
    case UndefinedDestructuringValue => tuplePrinter.write(WithMapG(SequencePrinter.undefinedTuple, value.map))
    case _ => Printer.fail(s"$value is not a tuple.")
  }
}

object SequencePrinter {
  val undefinedTuple: (UndefinedDestructuringValue.type, UndefinedDestructuringValue.type) =
    (UndefinedDestructuringValue, UndefinedDestructuringValue)
}