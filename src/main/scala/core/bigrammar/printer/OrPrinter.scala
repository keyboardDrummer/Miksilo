package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{Printer, State}
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Try}

class OrPrinter[T](first: Printer[T], second: Printer[T]) extends Printer[T] {
  override def write(from: WithMapG[T], state: State): Try[(State, ResponsiveDocument)] = {
    first.write(from, state).recoverWith({ case leftFailure: PrintError =>
      second.write(from, state).recoverWith({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  def combineOrFailures[U](left: PrintError, right: PrintError): Try[U] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)
}
