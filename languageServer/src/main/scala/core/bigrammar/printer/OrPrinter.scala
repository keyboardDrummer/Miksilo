package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.TryState
import core.responsiveDocument.ResponsiveDocument

import scala.util.Failure

class OrPrinter[T](first: Printer[T], second: Printer[T]) extends Printer[T] {
  override def write(from: WithMap[T], state: State): TryState[ResponsiveDocument] = {
    first.write(from, state).recoverWith[(State, ResponsiveDocument)]({ case leftFailure: PrintError =>
      second.write(from, state).recoverWith[(State, ResponsiveDocument)]({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  def combineOrFailures[U](left: PrintError, right: PrintError): TryState[ResponsiveDocument] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)
}
