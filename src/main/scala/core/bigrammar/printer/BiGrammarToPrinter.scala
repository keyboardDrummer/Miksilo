package core.bigrammar.printer

import core.bigrammar._
import core.bigrammar.grammars._
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

import scala.collection.mutable

object BiGrammarToPrinter {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    val printer = new BiGrammarToPrinter().toPrinterCached(grammar)
    printer.write(WithMapG(outerValue, Map.empty)).run(Map.empty).get._2
  }
}

class BiGrammarToPrinter {

  val printerCache: mutable.Map[BiGrammar, NodePrinter] = mutable.Map.empty

  def toPrinterCached(grammar: BiGrammar): NodePrinter = {

    printerCache.getOrElseUpdate(grammar, {
      val result: NodePrinter = grammar match {
        case choice: Choice => new OrPrinter(toPrinterCached(choice.left), toPrinterCached(choice.right))
        case custom: CustomGrammarWithoutChildren => custom
        case custom: CustomGrammar => new CachingPrinter(custom.createPrinter(toPrinterCached))
        case Keyword(keyword, _, verify) => (value) =>
          if (!verify || value.value == keyword)
            succeed(keyword)
          else
            failureToGrammar("keyword didn't match", grammar)
        case Delimiter(keyword) => _ => succeed(keyword)
        case labelled: Labelled => labelledToPrinter(labelled)
        case many: ManyHorizontal => new ManyPrinter(toPrinterCached(many.inner), (left, right) => left ~ right)
        case many: ManyVertical => new ManyPrinter(toPrinterCached(many.inner), (left, right) => left % right)
        case sequence: Sequence => new SequencePrinter(toPrinterCached(sequence.first), toPrinterCached(sequence.second),
          (left, right) => left ~ right)
        case topBottom: TopBottom => new SequencePrinter(toPrinterCached(topBottom.first), toPrinterCached(topBottom.second),
          (topDoc, bottomDoc) => topDoc % bottomDoc)
        case mapGrammar: MapGrammarWithMap => new MapGrammarWithMapPrinter(toPrinterCached(mapGrammar.inner), mapGrammar.deconstruct)
        case BiFailure(message) => _ => failureToGrammar(message, grammar)
        case valueGrammar: ValueGrammar => new ValuePrinter(valueGrammar.value)
        case Print(document) => _ => TryState.ret(document)
        case As(inner, key) => new AsPrinter(toPrinterCached(inner), key)
      }

      new NestPrinter(grammar, result)
    })
  }

  def succeed(value: ResponsiveDocument): TryState[ResponsiveDocument] = TryState.ret(value)

  def failureToGrammar(message: String, grammar: BiGrammar): TryState[ResponsiveDocument] = {
    Printer.fail("encountered failure", -10000)
  }

  class RedirectingPrinter extends NodePrinter {
    var inner: NodePrinter = _

    override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = inner.write(from)
  }

  def labelledToPrinter(labelled: Labelled): NodePrinter = {
    val redirectPrinter = new RedirectingPrinter()
    printerCache.put(labelled, new CachingPrinter(redirectPrinter))
    redirectPrinter.inner = toPrinterCached(labelled.inner)
    redirectPrinter
  }
}

/*
Used in destructuring when a value is required as a result but it's not in the object to be destructured.
*/
object UndefinedDestructuringValue //TODO looks a bit like ValueNotFound. Combine??
{
  override def toString = "_"
}