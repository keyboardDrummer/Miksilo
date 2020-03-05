package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

import scala.collection.mutable
import scala.util.Failure

class CachingPrinter(inner: NodePrinter) extends NodePrinter {
  val valueCache: mutable.Map[(Any, State), Printer.Result] = new mutable.HashMap
  val failure = Failure[(State, ResponsiveDocument)](NegativeDepthRootError(FoundDirectRecursionInLabel(inner), -1000))

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = state => {
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

  case class FoundDirectRecursionInLabel(name: NodePrinter) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }
}
