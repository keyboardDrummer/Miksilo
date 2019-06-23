package core.bigrammar.printer

import core.bigrammar.BiGrammarToParser.AnyWithMap
import core.bigrammar._
import core.bigrammar.grammars._
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.{ResponsiveDocument, ResponsiveLeftRight, ResponsiveTopBottom}

import scala.collection.mutable

object BiGrammarToPrinter {
  def toPrinter(grammar: BiGrammar): Any => ResponsiveDocument = {
    val printer = new BiGrammarToPrinter().toPrinterCached(grammar)
    outerValue => {
      val printResult = printer.write(WithMap(outerValue, Map.empty))
      printResult.run(Map.empty).get._2
    }
  }

  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    toPrinter(grammar)(outerValue)
  }
}

class BiGrammarToPrinter {

  val printerCache: mutable.Map[BiGrammar, NodePrinter] = mutable.Map.empty

  def toPrinterCached(grammar: BiGrammar): NodePrinter = {

    printerCache.getOrElseUpdate(grammar, {
      val result: NodePrinter = grammar match {
        case choice: BiChoice => new OrPrinter(toPrinterCached(choice.left), toPrinterCached(choice.right))
        case Delimiter(keyword, _) => _ => succeed(keyword)
        case Keyword(keyword, _, verify) => value =>
          if (!verify || value.value == keyword)
            succeed(keyword)
          else
            failureToGrammar("keyword didn't match", grammar)
        case custom: CustomGrammarWithoutChildren => custom
        case custom: CustomGrammar => new CachingPrinter(custom.createPrinter(toPrinterCached))
        case labelled: Labelled =>
          new NestPrinter(labelled.inner, labelledToPrinter(labelled))
        case many: ManyHorizontal => new ManyPrinter(toPrinterCached(many.inner), (left, right) => left ~ right)
        case many: ManyVertical => new ManyPrinter(toPrinterCached(many.inner), (left, right) => left % right)
        case sequence: BiSequence =>
          val inner = new SequencePrinter(toPrinterCached(sequence.first), toPrinterCached(sequence.second),
            (first, second) =>
              if (sequence.horizontal) ResponsiveLeftRight(first, second)
              else ResponsiveTopBottom(first, second))
          val deconstruct = (withMap: AnyWithMap) => sequence.bijective.destruct(withMap.value).map(v => WithMap(v, withMap.namedValues))
          new MapGrammarWithMapPrinter(inner, deconstruct)
        case mapGrammar: MapGrammarWithMap => new MapGrammarWithMapPrinter(toPrinterCached(mapGrammar.inner), mapGrammar.deconstruct)
        case BiFailure(message) => _ => failureToGrammar(message, grammar)
        case valueGrammar: ValueGrammar => new ValuePrinter(valueGrammar.value)
        case Print(document) => _ => TryState.value(document)
      }
      result
    })
  }

  def succeed(value: ResponsiveDocument): TryState[ResponsiveDocument] = TryState.value(value)

  def failureToGrammar(message: String, grammar: BiGrammar): TryState[ResponsiveDocument] = {
    Printer.fail("encountered failure", -10000)
  }

  class RedirectingPrinter(labelled: Labelled) extends NodePrinter {
    var inner: NodePrinter = _

    override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = inner.write(from)

    override def toString: String = labelled.toString
  }

  def labelledToPrinter(labelled: Labelled): NodePrinter = {
    val redirectPrinter = new RedirectingPrinter(labelled)
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