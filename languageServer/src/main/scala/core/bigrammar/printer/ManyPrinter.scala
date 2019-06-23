package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.{NodePrinter, TryState}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.Success

class ManyPrinter(val inner: NodePrinter,
                  val combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument)
  extends NodePrinter {
  val bindableInner = (value: WithMap[Any], state: State) =>
    inner.write(value, state).map(left => (left._1, (right: ResponsiveDocument) => combine(left._2, right)))

  override def write(from: WithMap[Any], state: State): TryState[ResponsiveDocument] = {
    val result: TryState[ResponsiveDocument] = from.value match {
      case seq: Seq[_] if seq.nonEmpty => new BindPrinter(bindableInner, this).
        write(WithMap((seq.head, seq.tail), from.namedValues), state) //TODO matching on both list and ArrayBuffer like this is a bit ugly.
      case _: Seq[_] => Success(state, Empty)
      case UndefinedDestructuringValue => Success(state, Empty)
      case _ => Printer.fail(s"$from passed to many was not a list")
    }
    result
  }
}
