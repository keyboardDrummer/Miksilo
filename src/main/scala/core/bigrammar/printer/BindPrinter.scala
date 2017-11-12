package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{NonePrintFailureException, Printer, State}
import core.grammar.~
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success}

class BindPrinter[T, U](first: TryState[T, ResponsiveDocument => ResponsiveDocument], second: Printer[U])
  extends Printer[~[T,U]] {
  override def write(from: WithMapG[~[T, U]], state: State) = {
    val firstValue = from.value._1
    val secondValue = from.value._2

    first.write(WithMapG(firstValue, from.map), state) match {
      case Success(firstSuccess) =>
        second.write(WithMapG(secondValue, from.map), firstSuccess._1) match {
          case Success(secondSuccess) => Success((secondSuccess._1, firstSuccess._2(secondSuccess._2)))
          case Failure(printError: PrintError) => Failure(printError.mapPartial(firstSuccess._2))
          case Failure(e: NonePrintFailureException) => throw e
          case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}
