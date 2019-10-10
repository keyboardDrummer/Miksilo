package core.bigrammar.printer

import core.bigrammar._
import core.bigrammar.grammars._
import core.bigrammar.printer.BiGrammarToPrinter.ToPrinterCached
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.{ResponsiveDocument, ResponsiveLeftRight, ResponsiveTopBottom}

import scala.collection.mutable

object BiGrammarToPrinter {
  def toPrinter[Value](grammar: BiGrammar[Value]): Value => ResponsiveDocument = {
    val printer = new BiGrammarToPrinter().toPrinterCached(grammar).asInstanceOf[Printer[WithMap[Value]]]
    outerValue => {
      val printResult = printer.write(WithMap(outerValue, Map.empty))
      printResult.run(Map.empty).get._2
    }
  }

  def toDocument[Value](outerValue: Value, grammar: BiGrammar[Value]): ResponsiveDocument = {
    toPrinter(grammar)(outerValue)
  }

  trait ToPrinterCached {
    def apply[Value](grammar: BiGrammar[Value]): Printer[Value]
  }
}

class BiGrammarToPrinter {

  val printerCache: mutable.Map[BiGrammar[_], Printer[_]] = mutable.Map.empty

  val ToPrinterCached = new ToPrinterCached() {
    override def apply[Value](grammar: BiGrammar[Value]) = toPrinterCached(grammar)
  }

  def toPrinterCached[Value](grammar: BiGrammar[Value]): Printer[Value] = {

    printerCache.getOrElseUpdate(grammar, {
      val result: Printer[_] = grammar match {
        case choice: BiChoice[Value] => new OrPrinter(toPrinterCached(choice.left), toPrinterCached(choice.right))
        case delimiter: Delimiter => _ => succeed(delimiter.value)
        case Keyword(keyword, _) => value =>
            succeed(keyword)
        case custom: CustomGrammarWithoutChildren[Value] => custom
        case custom: CustomGrammar[Value] => new CachingPrinter(custom.createPrinter(ToPrinterCached))
        case labelled: Labelled[Value] =>
          new NestPrinter(labelled.inner, labelledToPrinter(labelled))

        case many: ManyHorizontal[_] => // TODO it would be awesome of we could capture the type argument of many here, but Scala doesn't seem to allow that.
          new ManyPrinter(toPrinterCached(many.inner.asInstanceOf[BiGrammar[Any]]), (left, right) => left ~ right)
        case many: ManyVertical[_] => new ManyPrinter(toPrinterCached(many.inner.asInstanceOf[BiGrammar[Any]]), (left, right) => left % right)
        case sequence: BiSequence[_,_, Value] =>
          val inner = new SequencePrinter(toPrinterCached(sequence.first), toPrinterCached(sequence.second),
            (first, second) =>
              if (sequence.horizontal) ResponsiveLeftRight(first, second)
              else ResponsiveTopBottom(first, second))
          val deconstruct = (value: Value) => sequence.bijective.destruct(value)
          new MapGrammarWithMapPrinter(inner, deconstruct)
        case mapGrammar: MapGrammar[_, Value] =>
          new MapGrammarWithMapPrinter(toPrinterCached(mapGrammar.inner), mapGrammar.deconstruct)
        case BiFailure(message) => _ => failureToGrammar(message, grammar)
        case valueGrammar: ValueGrammar[Value] => new ValuePrinter(valueGrammar.value)
        case Print(document) => _ => TryState.value(document)
      }
      result
    })
  }.asInstanceOf[Printer[Value]]

  def succeed(value: ResponsiveDocument): TryState[ResponsiveDocument] = TryState.value(value)

  def failureToGrammar(message: String, grammar: BiGrammar[_]): TryState[ResponsiveDocument] = {
    Printer.fail("encountered failure", -10000)
  }

  class RedirectingPrinter[Value](labelled: Labelled[Value]) extends Printer[Value] {
    var inner: Printer[Value] = _

    override def write(from: Value): TryState[ResponsiveDocument] = inner.write(from)

    override def toString: String = labelled.toString
  }

  def labelledToPrinter[Value](labelled: Labelled[Value]): Printer[Value] = {
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