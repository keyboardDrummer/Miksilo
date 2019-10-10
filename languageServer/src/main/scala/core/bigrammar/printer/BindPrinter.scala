package core.bigrammar.printer

import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NonePrintFailureException
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success}

class BindPrinter[T, U](first: T => TryState[ResponsiveDocument => ResponsiveDocument], second: Printer[U])
  extends Printer[(T,U)] {
  override def write(from: (T, U)): TryState[ResponsiveDocument] = state => {
    val firstValue = from._1
    val secondValue = from._2

    first(firstValue).run(state) match {
      case Success(firstSuccess) =>
        second.write(secondValue).run(firstSuccess._1) match {
          case Success(secondSuccess) => Success((secondSuccess._1, firstSuccess._2(secondSuccess._2)))
          case Failure(printError: PrintError) => Failure(MappedError(firstSuccess._2, printError))
          case Failure(e: NonePrintFailureException) => throw e
          case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}
