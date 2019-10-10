package core.bigrammar.printer

import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

class SequencePrinter[Left, Right](first: Printer[Left], second: Printer[Right],
                      combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument) extends Printer[(Left, Right)] {
  private val newFirst = (value: Left) => first.write(value).map(
    firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

  val tuplePrinter = new BindPrinter[Left, Right](newFirst, second)

  override def write(value: (Left, Right)): TryState[ResponsiveDocument] = tuplePrinter.write(value)
}

object SequencePrinter {
  val undefinedTuple: (UndefinedDestructuringValue.type, UndefinedDestructuringValue.type) =
    (UndefinedDestructuringValue, UndefinedDestructuringValue)
}