package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.Printer.NonePrintFailureException
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success}

class BindPrinter[T, U](first: WithMapG[T] => TryState[ResponsiveDocument => ResponsiveDocument], second: Printer[U])
  extends Printer[(T,U)] {
  override def write(from: WithMapG[(T, U)]): TryState[ResponsiveDocument] = state => {
    val firstValue = from.value._1
    val secondValue = from.value._2

    first(WithMapG(firstValue, from.map)).run(state) match {
      case Success(firstSuccess) =>
        second.write(WithMapG(secondValue, from.map)).run(firstSuccess._1) match {
          case Success(secondSuccess) => Success((secondSuccess._1, firstSuccess._2(secondSuccess._2)))
          case Failure(printError: PrintError) => Failure(MappedError(firstSuccess._2, printError))
          case Failure(e: NonePrintFailureException) => throw e
          case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}
