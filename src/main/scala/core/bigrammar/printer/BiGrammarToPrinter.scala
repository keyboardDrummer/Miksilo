package core.bigrammar.printer

import core.bigrammar._
import core.bigrammar.grammars._
import core.bigrammar.printer.TryState._
import core.grammar.~
import core.responsiveDocument.ResponsiveDocument

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object BiGrammarToPrinter {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    val printer = new BiGrammarToPrinter().toPrinterCached(grammar)
    printer.write(WithMapG(outerValue, Map.empty), Map.empty).get._2
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
        case Keyword(keyword, _, verify) => (value, state) =>
          if (!verify || value.value == keyword)
            succeed(state, keyword)
          else
            failureToGrammar("keyword didn't match", grammar)
        case Delimiter(keyword) => (value, state) => succeed(state, keyword)
        case labelled: Labelled => labelledToPrinter(labelled)
        case many: ManyHorizontal => new ManyPrinter(toPrinterCached(many.inner), (left, right) => left ~ right)
        case many: ManyVertical => new ManyPrinter(toPrinterCached(many.inner), (left, right) => left % right)
        case sequence: Sequence => sequenceToPrinter(sequence, (left, right) => left ~ right)
        case topBottom: TopBottom => sequenceToPrinter(topBottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
        case mapGrammar: MapGrammarWithMap => new MapGrammarWithMapPrinter(toPrinterCached(mapGrammar.inner), mapGrammar.deconstruct)
        case BiFailure(message) => (value, state) => failureToGrammar(message, grammar)
        case valueGrammar: ValueGrammar => new ValuePrinter(valueGrammar.value)
        case Print(document) => (value, state) => succeed(state, document)
        case As(inner, key) => new AsPrinter(toPrinterCached(inner), key)
      }

      new NestPrinter(grammar, result)
    })
  }

  def succeed(state: State, value: ResponsiveDocument): Result = Success((state, value))

  def failureToGrammar(message: String, grammar: BiGrammar): Failure[Nothing] = {
    fail("encountered failure", -10000)
  }

  class RedirectingPrinter extends NodePrinter {
    var inner: NodePrinter = _

    override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = inner.write(from, state)
  }

  def labelledToPrinter(labelled: Labelled): NodePrinter = {
    val redirectPrinter = new RedirectingPrinter()
    printerCache.put(labelled, new CachingPrinter(redirectPrinter))
    redirectPrinter.inner = toPrinterCached(labelled.inner)
    redirectPrinter
  }

  val undefinedTuple = core.grammar.~[Any, Any](UndefinedDestructuringValue, UndefinedDestructuringValue)

  def sequenceToPrinter(grammar: SequenceLike,
                        combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): NodePrinter = {
    val newFirst = toPrinterCached(grammar.first).map(
      firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

    val tuplePrinter = new BindPrinter[Any, Any](newFirst, toPrinterCached(grammar.second))

    new NodePrinter {
      override def write(value: WithMapG[Any], state: State) = {
        val result = value.value match {
          case tuple: ~[Any, Any] => tuplePrinter.write(WithMapG(tuple, value.map), state)
          case UndefinedDestructuringValue => tuplePrinter.write(WithMapG(undefinedTuple, value.map), state)
          case _ => fail(s"$value is not a tuple.")
        }
        result
      }
    }
  }
}

/*
Used in destructuring when a value is required as a result but it's not in the object to be destructured.
*/
object UndefinedDestructuringValue //TODO looks a bit like ValueNotFound. Combine??
{
  override def toString = "_"
}