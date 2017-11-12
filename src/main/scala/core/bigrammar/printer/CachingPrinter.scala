package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{NodePrinter, Result, State, fail}

import scala.collection.mutable

class CachingPrinter(inner: NodePrinter) extends NodePrinter {
  val valueCache: mutable.Map[(Any, State), Result] = mutable.Map.empty

  override def write(from: WithMapG[Any], state: State): Result = {
    val key = (from, state)
    valueCache.get(key) match {
      case Some(result) =>
        result
      case _ =>
        valueCache.put(key, fail(FoundDirectRecursionInLabel(inner), -1000))
        val result = inner.write(from, state)
        valueCache.put(key, result)
        result
    }
  }

  case class FoundDirectRecursionInLabel(name: NodePrinter) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }
}
