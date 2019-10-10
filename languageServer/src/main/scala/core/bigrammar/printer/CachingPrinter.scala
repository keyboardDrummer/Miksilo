package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

import scala.collection.mutable
import scala.util.Failure

class CachingPrinter[Value](inner: Printer[Value]) extends Printer[Value] {
  val valueCache: mutable.Map[(Any, State), Printer.Result] = mutable.Map.empty
  val failure = Failure[(State, ResponsiveDocument)](NegativeDepthRootError(FoundDirectRecursionInLabel(inner), -1000))

  override def write(from: Value): TryState[ResponsiveDocument] = state => {
    val key = (from, state)
    valueCache.get(key) match {
      case Some(result) =>
        result
      case _ =>
        valueCache.put(key, failure)
        val result = inner.write(from).run(state)
        valueCache.put(key, result)
        result
    }
  }

  case class FoundDirectRecursionInLabel(name: Printer[Value]) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }
}
