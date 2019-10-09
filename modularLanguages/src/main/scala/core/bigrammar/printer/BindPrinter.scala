package core.bigrammar.printer

import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success}

class BindPrinter[T, U](first: WithMap[T] => TryState[ResponsiveDocument => ResponsiveDocument], second: Printer[U])
  extends Printer[(T,U)] {
  override def write(from: WithMap[(T, U)]): TryState[ResponsiveDocument] = state => {
    val firstValue = from.value._1
    val secondValue = from.value._2

    first(WithMap(firstValue, from.namedValues)).run(state) match {
      case Success(firstSuccess) =>
        second.write(WithMap(secondValue, from.namedValues)).run(firstSuccess._1) match {
          case Success(secondSuccess) => Success((secondSuccess._1, firstSuccess._2(secondSuccess._2)))
          case Failure(printError: PrintError) => Failure(MappedError(firstSuccess._2, printError))
          case Failure(e: NonePrintFailureException) => throw e
          case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}
