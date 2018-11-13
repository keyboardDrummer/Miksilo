package core.bigrammar.printer

import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

class SequencePrinter(first: NodePrinter, second: NodePrinter,
                      combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument) extends NodePrinter {
  private val newFirst = (value: WithMap[Any]) => first.write(value).map(
    firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

  val tuplePrinter = new BindPrinter[Any, Any](newFirst, second)

  override def write(value: WithMap[Any]): TryState[ResponsiveDocument] = value.value match {
    case tuple: (Any, Any) => tuplePrinter.write(WithMap(tuple, value.map))
    case UndefinedDestructuringValue => tuplePrinter.write(WithMap(SequencePrinter.undefinedTuple, value.map))
    case _ => Printer.fail(s"$value is not a tuple.")
  }
}

object SequencePrinter {
  val undefinedTuple: (UndefinedDestructuringValue.type, UndefinedDestructuringValue.type) =
    (UndefinedDestructuringValue, UndefinedDestructuringValue)
}