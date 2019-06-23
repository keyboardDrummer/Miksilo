package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.{NodePrinter, TryState}
import core.responsiveDocument.ResponsiveDocument

class SequencePrinter(first: NodePrinter, second: NodePrinter,
                      combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument) extends NodePrinter {
  private val newFirst = (value: WithMap[Any], state: State) => first.write(value, state).map(
    firstValue => (firstValue._1, (secondValue: ResponsiveDocument) => combine(firstValue._2, secondValue)))

  val tuplePrinter = new BindPrinter[Any, Any](newFirst, second)

  override def write(value: WithMap[Any], state: State): TryState[ResponsiveDocument] = value.value match {
    case tuple: (Any, Any) => tuplePrinter.write(WithMap(tuple, value.namedValues), state)
    case UndefinedDestructuringValue => tuplePrinter.write(WithMap(SequencePrinter.undefinedTuple, value.namedValues), state)
    case _ => Printer.fail(s"$value is not a tuple.")
  }
}

object SequencePrinter {
  val undefinedTuple: (UndefinedDestructuringValue.type, UndefinedDestructuringValue.type) =
    (UndefinedDestructuringValue, UndefinedDestructuringValue)
}