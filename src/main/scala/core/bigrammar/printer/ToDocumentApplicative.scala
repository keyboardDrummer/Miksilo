package core.bigrammar.printer

import core.responsiveDocument.ResponsiveDocument

import scala.util.{Success, Failure, Try}

object ToDocumentApplicative {

  def or(first: Try[ResponsiveDocument], second: Try[ResponsiveDocument]): Try[ResponsiveDocument] = {
    first.recoverWith({ case leftFailure: PrintError =>
      second.recoverWith({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  private def combineOrFailures(left: PrintError, right: PrintError): Try[ResponsiveDocument] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)

  def bind(first: Try[ResponsiveDocument => ResponsiveDocument], second: Try[ResponsiveDocument]): Try[ResponsiveDocument] = {
    first match {
      case Success(f) => second match {
        case Success(secondSuccess) => Success(f(secondSuccess))
        case Failure(printError: PrintError) => Failure(printError.mapPartial(f))
        case Failure(e: NonePrintFailureException) => throw e
        case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}

class NonePrintFailureException(e: Throwable) extends RuntimeException {
  override def toString = "failed toDocument with something different than a print failure: " + e.toString
}
