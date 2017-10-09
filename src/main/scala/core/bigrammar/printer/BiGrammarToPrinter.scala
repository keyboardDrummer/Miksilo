package core.bigrammar.printer

import java.util.Objects

import core.bigrammar._
import core.bigrammar.printer.PrinterTypes.NodePrinter
import core.document.Empty
import core.grammar.~
import core.responsiveDocument.ResponsiveDocument
import PrinterTypes._
import scala.util.{Failure, Success, Try}

object BiGrammarToPrinter {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    val printer = new BiGrammarToPrinter().toPrinterCached(grammar)
    printer(outerValue)(Map.empty).get._2
  }
}

trait TryState[From, To] {
  def write(from: From, state: State): Try[(State, To)]
}

object PrinterTypes {

  type State = Map[Any, Any]
  type TryState[T, U] = T => State => Try[(State, U)]
  type Printer[T] = TryState[T, ResponsiveDocument]
  type NodePrinter = Printer[Any]

  def map[T, U, V](tryState: TryState[T, U], function: U => V): TryState[T, V] = {
    value: T => state => tryState(value)(state).map(t => (t._1, function(t._2)))
  }

  def bind[T, U](first: TryState[T, ResponsiveDocument => ResponsiveDocument],
                 second: Printer[U]): Printer[~[T,U]] = {
    tuple => state => {
      val firstValue = tuple._1
      val secondValue = tuple._2

      first(firstValue)(state) match {
        case Success(firstSuccess) => second(secondValue)(firstSuccess._1) match {
          case Success(secondSuccess) => Success(secondSuccess._1, firstSuccess._2(secondSuccess._2))
          case Failure(printError: PrintError) => Failure(printError.mapPartial(firstSuccess._2))
          case Failure(e: NonePrintFailureException) => throw e
          case Failure(e: Throwable) => throw new NonePrintFailureException(e)
        }
        case failure: Failure[_] => Failure(failure.exception)
      }
    }
  }

  class NonePrintFailureException(e: Throwable) extends RuntimeException {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def or[T](first: Printer[T], second: Printer[T]): Printer[T] = value => state => {
    first(value)(state).recoverWith({ case leftFailure: PrintError =>
      second(value)(state).recoverWith({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  private def combineOrFailures[T](left: PrintError, right: PrintError): Try[T] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)


  def fail(inner: Any, depth: Int = 0) = Failure(RootError(depth, Empty, inner))
}

class BiGrammarToPrinter {

  case class LabelWithValue(label: Labelled, value: Any)
  var printerCache: Map[AnyRef, NodePrinter] = Map.empty

  val empty: Try[(State, ResponsiveDocument)] = Success((Map.empty, Empty))

  def toPrinterCached(grammar: BiGrammar): NodePrinter = {
    def nestError(printer: NodePrinter): NodePrinter = value => state =>
      printer(value)(state).recoverWith(
      { case e: PrintError => Failure(NestedError((value,state), grammar, e))})

    val result: NodePrinter = grammar match {
      case choice:Choice => or(toPrinterCached(choice.left), toPrinterCached(choice.right))
      case custom:CustomGrammar => value => state => custom.print(WithMap(value, state)).map(v => (state, v))

      case Keyword(keyword, _, verify) => value => state =>
        if (!verify || value == keyword)
          Try((state, keyword))
        else
          failureToGrammar("keyword didn't match", grammar)
      case Delimiter(keyword) => value => state => Try((state, keyword))
      case labelled: Labelled => labelledToPrinter(labelled)
      case many: ManyHorizontal => manyToPrinter(many, (left, right) => left ~ right)
      case many: ManyVertical => manyToPrinter(many, (left, right) => left % right)
      case sequence: Sequence => sequenceToPrinter(sequence, (left, right) => left ~ right)
      case topBottom: TopBottom => sequenceToPrinter(topBottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
      case mapGrammar: MapGrammar => mapGrammarToPrinter(mapGrammar)
      case BiFailure(message) => value => state => failureToGrammar(message, grammar)
      case valueGrammar: ValueGrammar => valueGrammarToPrinter(valueGrammar)
      case Print(document) => _ => state => Try((state, document))
      case As(inner, key) => value => state =>
        if (state.contains(key)) toPrinterCached(inner)(state(key))(state)
        else fail(s"did not find as key $key in state $state")
    }

    nestError(result)
  }

  def failureToGrammar(message: String, grammar: BiGrammar): Failure[Nothing] = {
    fail("encountered failure", -10000)
  }

  def mapGrammarToPrinter(mapGrammar: MapGrammar): NodePrinter = {
    val innerPrinter = toPrinterCached(mapGrammar.inner)
    value => state => {
        for {
          (newState, deconstructedValue) <- deconstructValue(value, state, mapGrammar)
          result <- innerPrinter(deconstructedValue)(newState).recoverWith { case e: PrintError => Failure(e.mapPartial(x => x)) }
        } yield result
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

  case class FoundDirectRecursionInLabel(name: Labelled) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }

  def labelledToPrinter(labelled: Labelled): NodePrinter = {
    val key = labelled.name
    val printerOption = printerCache.get(key)
    printerOption match {
      case Some(printer) => printer
      case None =>
        val oldCache = printerCache
        val failPrinter: NodePrinter = _ => _ => fail(FoundDirectRecursionInLabel(labelled), -1000)
        printerCache += key -> failPrinter
        val result = toPrinterCached(labelled.inner)
        printerCache = oldCache + (key -> result)
        result
    }
  }

  def sequenceToPrinter(grammar: SequenceLike,
                        combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): NodePrinter = {
    val newFirst = map[Any, ResponsiveDocument, ResponsiveDocument => ResponsiveDocument](
      toPrinterCached(grammar.first),
      firstValue => (secondValue: ResponsiveDocument) => combine(firstValue, secondValue))

    val bindResult = bind[Any, Any](newFirst, toPrinterCached(grammar.second))

    {
      case tuple: ~[Any, Any] => bindResult(tuple)
      case UndefinedDestructuringValue => bindResult(core.grammar.~[Any, Any](UndefinedDestructuringValue, UndefinedDestructuringValue))
      case value => _ => fail(s"$value is not a tuple.")
    }
  }

  def manyToPrinter(many: Many,
                    combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): NodePrinter = {

    val innerPrinter: NodePrinter = toPrinterCached(many.inner)
    val bindableInner = map[Any, ResponsiveDocument, ResponsiveDocument => ResponsiveDocument](
        innerPrinter, left => (right: ResponsiveDocument) => combine(left, right))

    lazy val seqPrinter: Printer[List[Any]] = value => state => value match {
      case head :: tail => bind(bindableInner, seqPrinter)(core.grammar.~(head, tail))(state)
      case _ => Success(state, Empty)
    }

    {
      case list: List[Any] => seqPrinter(list)
      case value => _ => fail(s"$value passed to many was not a list")
    }
  }

  case class ProduceWithDifferentValue(actual: Any, expected: Any) {
    override def toString = s"given value $actual was not equal to value grammar's $expected"
  }

  def valueGrammarToPrinter(grammar: ValueGrammar): NodePrinter = value => state => {
    if (Objects.equals(grammar.value, value)) Success((Map.empty, Empty))
    else fail(ProduceWithDifferentValue(value, grammar.value), -100)
  }
}
