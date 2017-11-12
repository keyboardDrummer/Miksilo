package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{NodePrinter, State, fail}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Success, Try}

class ManyPrinter(val inner: NodePrinter,
                  val combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument)
  extends NodePrinter {
  val bindableInner = inner.map(left => (right: ResponsiveDocument) => combine(left, right))

  override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {
    val result: Try[(State, ResponsiveDocument)] = from.value match {
      case seq: Seq[_] if seq.nonEmpty => new BindPrinter(bindableInner, this).
        write(WithMapG(core.grammar.~(seq.head, seq.tail), from.map), state) //TODO matching on both list and ArrayBuffer like this is a bit ugly.
      case seq: Seq[_] => Success(state, Empty)
      case _ => fail(s"$from passed to many was not a list")
    }
    result
  }
}
