package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.modularLanguages.core.bigrammar.WithMap
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

class SequencePrinter(first: NodePrinter, second: NodePrinter,
                      combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument) extends NodePrinter {
  private val newFirst = (value: WithMap[Any]) => first.write(value).map(
    firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

  val tuplePrinter = new BindPrinter[Any, Any](newFirst, second)

  override def write(value: WithMap[Any]): TryState[ResponsiveDocument] = value.value match {
    case tuple: (Any, Any) => tuplePrinter.write(WithMap(tuple, value.namedValues))
    case UndefinedDestructuringValue => tuplePrinter.write(WithMap(SequencePrinter.undefinedTuple, value.namedValues))
    case _ => Printer.fail(s"$value is not a tuple.")
  }
}

object SequencePrinter {
  val undefinedTuple: (UndefinedDestructuringValue.type, UndefinedDestructuringValue.type) =
    (UndefinedDestructuringValue, UndefinedDestructuringValue)
}