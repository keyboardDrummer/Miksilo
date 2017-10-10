package core.bigrammar.printer

import java.util.Objects

import core.bigrammar._
import core.bigrammar.printer.TryState._
import core.document.Empty
import core.grammar.~
import core.responsiveDocument.ResponsiveDocument

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object BiGrammarToPrinter {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    val printer = new BiGrammarToPrinter().toPrinterCached(grammar)
    printer.write(outerValue, Map.empty).get._2
  }
}

class BindPrinter[T, U](first: TryState[T, ResponsiveDocument => ResponsiveDocument], second: Printer[U])
  extends Printer[~[T,U]] {
  override def write(from: ~[T, U], state: State) = {
    val firstValue = from._1
    val secondValue = from._2

    first.write(firstValue, state) match {
      case Success(firstSuccess) =>
        second.write(secondValue, firstSuccess._1) match {
          case Success(secondSuccess) => Success((secondSuccess._1, firstSuccess._2(secondSuccess._2)))
          case Failure(printError: PrintError) => Failure(printError.mapPartial(firstSuccess._2))
          case Failure(e: NonePrintFailureException) => throw e
          case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}

object TryState {

  type Printer[T] = TryState[T, ResponsiveDocument]
  type NodePrinter = Printer[Any]
  type State = Map[Any, Any]

  class NonePrintFailureException(e: Throwable) extends RuntimeException {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def or[T](first: Printer[T], second: Printer[T]): Printer[T] = (value: T,  state) => {
    first.write(value, state).recoverWith({ case leftFailure: PrintError =>
      second.write(value, state).recoverWith({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  def combineOrFailures[T](left: PrintError, right: PrintError): Try[T] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)

  def fail(inner: Any, depth: Int = 0) = Failure(RootError(depth, Empty, inner))
}

trait TryState[From, To] {
  def write(from: From, state: State): Try[(State, To)]

  def map[NewTo](function: To => NewTo): TryState[From, NewTo] = (from: From, state: State) =>
    write(from, state).map[(State, NewTo)](t => (t._1, function(t._2)))
}

class BiGrammarToPrinter {

  case class LabelWithValue(label: Labelled, value: Any)
  val printerCache: mutable.Map[BiGrammar, NodePrinter] = mutable.Map.empty
  val valueCache: mutable.Map[(NodePrinter, Any, State), Try[(State, ResponsiveDocument)]] = mutable.Map.empty

  class CachingPrinter(inner: NodePrinter) extends NodePrinter {
    override def write(from: Any, state: State): Try[(State, ResponsiveDocument)] = {
      val key = (inner, from, state)
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
  }

  class NestPrinter(grammar: BiGrammar, inner: NodePrinter) extends NodePrinter {
    override def write(from: Any, state: State): Try[(State, ResponsiveDocument)] = {
      inner.write(from, state).recoverWith(
        { case e: PrintError => Failure(NestedError((from, state), grammar, e))})
    }
  }

  def toPrinterCached(grammar: BiGrammar): NodePrinter = {

    printerCache.getOrElseUpdate(grammar, {
      val result: NodePrinter = grammar match {
        case choice:Choice => or(toPrinterCached(choice.left), toPrinterCached(choice.right))
        case custom:CustomGrammar => custom
        case Keyword(keyword, _, verify) => (value, state) =>
          if (!verify || value == keyword)
            Try((state, keyword))
          else
            failureToGrammar("keyword didn't match", grammar)
        case Delimiter(keyword) => (value, state) => Try((state, keyword))
        case labelled: Labelled => labelledToPrinter(labelled)
        case many: ManyHorizontal => new ManyPrinter(many, (left, right) => left ~ right)
        case many: ManyVertical => new ManyPrinter(many, (left, right) => left % right)
        case sequence: Sequence => sequenceToPrinter(sequence, (left, right) => left ~ right)
        case topBottom: TopBottom => sequenceToPrinter(topBottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
        case mapGrammar: MapGrammar => mapGrammarToPrinter(mapGrammar)
        case BiFailure(message) => (value, state) => failureToGrammar(message, grammar)
        case valueGrammar: ValueGrammar => valueGrammarToPrinter(valueGrammar)
        case Print(document) => (value, state) => Try((state, document))
        case As(inner, key) =>
          val innerPrinter = toPrinterCached(inner)
          new NodePrinter {
            override def write(from: Any, state: State) = {
              if (state.contains(key)) innerPrinter.write(state(key), state - key)
              else fail(s"did not find as key $key in state $state")
            }
          }
      }

      new NestPrinter(grammar, new CachingPrinter(result))
    })
  }



  def failureToGrammar(message: String, grammar: BiGrammar): Failure[Nothing] = {
    fail("encountered failure", -10000)
  }

  def mapGrammarToPrinter(mapGrammar: MapGrammar): NodePrinter = {
    val innerPrinter = toPrinterCached(mapGrammar.inner)
    (value, state) => {
        for {
          (newState, deconstructedValue) <- deconstructValue(value, state, mapGrammar)
          result <- innerPrinter.write(deconstructedValue, newState).recoverWith { case e: PrintError => Failure(e.mapPartial(x => x)) }
        } yield (state, result._2) //TODO scary not to use the state from result
      }
  }

  def deconstructValue(value: Any, state: State, grammar: MapGrammar): Try[(State, Any)] = {
    if (grammar.showMap) {
      grammar.deconstruct(WithMap(value, state)) match {
        case Some(WithMap(newValue, newState)) =>
          Try((newState, newValue))
        case _ => fail("could not deconstruct value")
      }
    }
    else {
      grammar.deconstruct(value) match {
        case Some(x) => Try((state, x))
        case None => fail("could not deconstruct value")
      }
    }
  }

  case class FoundDirectRecursionInLabel(name: NodePrinter) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }

  def labelledToPrinter(labelled: Labelled): NodePrinter = {
    val printerOption = printerCache.get(labelled)
    printerOption match {
      case Some(printer) => printer
      case None =>
        var result: NodePrinter = null
        val redirectPrinter: NodePrinter = (value, state) => result.write(value, state)
        printerCache.put(labelled, redirectPrinter)
        result = toPrinterCached(labelled.inner)
        printerCache.put(labelled, result)
        result
    }
  }

  val undefinedTuple = core.grammar.~[Any, Any](UndefinedDestructuringValue, UndefinedDestructuringValue)
  def sequenceToPrinter(grammar: SequenceLike,
                        combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): NodePrinter = {
    val newFirst = toPrinterCached(grammar.first).map(
      firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

    val tuplePrinter = new BindPrinter[Any, Any](newFirst, toPrinterCached(grammar.second))

    (value, state) => {
      val result = value match {
        case tuple: ~[Any, Any] => tuplePrinter.write(tuple, state)
        case UndefinedDestructuringValue => tuplePrinter.write(undefinedTuple, state)
        case _ => fail(s"$value is not a tuple.")
      }
      result
    }
  }

  class ManyPrinter(val many: Many,
                    val combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument) extends NodePrinter {
    val innerPrinter: NodePrinter = toPrinterCached(many.inner)
    val bindableInner = innerPrinter.map(left => (right: ResponsiveDocument) => combine(left, right))

    override def write(from: Any, state: State): Try[(State, ResponsiveDocument)] = {
      val result: Try[(State, ResponsiveDocument)] = from match {
        case seq: Seq[_] if seq.nonEmpty => new BindPrinter(bindableInner, this).write(core.grammar.~(seq.head, seq.tail), state) //TODO matching on both list and ArrayBuffer like this is a bit ugly.
        case seq: Seq[_] => Success(state, Empty)
        case _ => fail(s"$from passed to many was not a list")
      }
      result
    }
  }

  case class ValueDiffers(actual: Any, expected: Any) {
    override def toString = s"given value $actual was not equal to value grammar's $expected"
  }

  def valueGrammarToPrinter(grammar: ValueGrammar): NodePrinter = (value, state) => {
    if (Objects.equals(grammar.value, value)) Success(state, Empty)
    else fail(ValueDiffers(value, grammar.value), -100)
  }
}
