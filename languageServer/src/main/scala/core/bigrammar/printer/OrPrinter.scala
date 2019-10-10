package core.bigrammar.printer

import core.bigrammar.WithMap
import core.responsiveDocument.ResponsiveDocument

import scala.util.Failure

class OrPrinter[T](first: Printer[T], second: Printer[T]) extends Printer[T] {
  override def write(from: T): TryState[ResponsiveDocument] = {
    first.write(from).recoverWith[ResponsiveDocument]({ case leftFailure: PrintError =>
      second.write(from).recoverWith[ResponsiveDocument]({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  def combineOrFailures[U](left: PrintError, right: PrintError): TryState[ResponsiveDocument] =
    state => if (left.depth >= right.depth) Failure(left) else Failure(right)
}
