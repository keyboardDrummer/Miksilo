package core.bigrammar

import java.util.Objects

import core.document.Empty
import core.grammar.{StringLiteral, ~}
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success, Try}

case class LabelWithValue(label: Labelled, value: Any)

object BiGrammarToDocument {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    new BiGrammarToDocument().toDocumentCached(outerValue, grammar).get
  }
}

class BiGrammarToDocument {
  var cache: Map[LabelWithValue, Try[ResponsiveDocument]] = Map.empty
  
  def toDocumentCached(value: Any, grammar: BiGrammar): Try[ResponsiveDocument] = {
    def nestError(result: Try[ResponsiveDocument]) = result.recoverWith[ResponsiveDocument]({case e: PrintError => Failure(NestedError(value ,grammar, e))})

    val result: Try[ResponsiveDocument] = grammar match {
      case Choice(first, second) => ToDocumentApplicative.or(toDocumentCached(value, second), toDocumentCached(value, first))
      case Consume(StringLiteral) => Try("\"" + value + "\"")
      case Consume(consume) => Try(value.toString)
      case Keyword(keyword, _) => Try(keyword)
      case Delimiter(keyword) => Try(keyword)
      case labelled: Labelled => labelToDocument(value, labelled)
      case many: ManyHorizontal => foldSequence(value, many.inner, (left, right) => left ~ right)
      case many: ManyVertical => foldSequence(value, many.inner, (left, right) => left % right)
      case sequence: Sequence => foldProduct(value, sequence, (left, right) => left ~ right)
      case topBottom: TopBottom => foldProduct(value, topBottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
      case mapGrammar: MapGrammar => mapGrammarToDocument(value, mapGrammar)
      case BiFailure => failureToGrammar(value, grammar)
      case Produce(producedValue) => produceToDocument(value, grammar, producedValue)
      case Print(document) => Try(document)
    }

    nestError(result)
  }

  object EncounteredFailure extends Throwable
  def failureToGrammar(value: Any, grammar: BiGrammar): Failure[Nothing] = {
    emptyFailure(value, EncounteredFailure, grammar, -10000)
  }

  def mapGrammarToDocument(value: Any, mapGrammar: MapGrammar): Try[ResponsiveDocument] = {
    for {
      deconstructedValue <- deconstructValue(value, mapGrammar)
      result <- toDocumentCached(deconstructedValue, mapGrammar.inner)
    } yield result
  }

  case class FoundDirectRecursionInLabel(name: Labelled) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }

  def labelToDocument(value: Any, labelled: Labelled): Try[ResponsiveDocument] = {
    val key = new LabelWithValue(labelled, value)
    val maybeCachedValue = cache.get(key)
    maybeCachedValue match {
      case Some(cachedValue) => cachedValue
      case None =>
        val oldCache = cache
        cache += key -> emptyFailure(value, FoundDirectRecursionInLabel(labelled), labelled, -1000)
        val result = toDocumentCached(value, labelled.inner)
        cache = oldCache + (key -> result)
        result
    }
  }


  def foldProduct(value: Any, grammar: SequenceLike,
                  combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    for {
      ~(firstValue, secondValue) <- extractProduct(value, grammar)
      firstDocument = toDocumentCached(firstValue, grammar.first)
      secondDocument = toDocumentCached(secondValue, grammar.second)
      result <- combineTwo(firstDocument, secondDocument, combine)
    } yield result
  }

  def foldSequence(value: Any, inner: BiGrammar,
                   combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    for {
      valueSequence <- extractSequence(value)
      innerDocuments = valueSequence.map(element => toDocumentCached(element, inner))
      result <- innerDocuments.foldRight[Try[ResponsiveDocument]](Success(Empty))((result, element) => combineTwo(result, element, combine))
    } yield result
  }

  case class FoundProduceWithNotEqualValue(expected: Any) extends Throwable {
    override def toString = s"value was not equal to produce value $expected"
  }
  def produceToDocument(value: Any, grammar: BiGrammar, producedValue: Any): Try[ResponsiveDocument] = {
    if (Objects.equals(producedValue, value)) Try(Empty)
    else emptyFailure(value, FoundProduceWithNotEqualValue(producedValue), grammar, -100)
  }

  object CouldNotDeconstructValue extends Throwable
  def deconstructValue(value: Any, grammar: MapGrammar): Try[Any] = {
    grammar.deconstruct(value) match {
      case Some(x) => Try(x)
      case None => emptyFailure(value, CouldNotDeconstructValue, grammar)
    }
  }

  def emptyFailure(value: Any, inner: Throwable, grammar: BiGrammar = null, depth: Int = 0) =
    Failure(RootError(depth, Empty, value, grammar, inner))

  def combineTwo(first: Try[ResponsiveDocument], second: => Try[ResponsiveDocument],
                 combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    ToDocumentApplicative.bind(first.map(firstDoc => secondDoc => combine(firstDoc, secondDoc)), second)
  }

  def extractSequence(value: Any): Try[Seq[Any]] = {
    Try.apply(value.asInstanceOf[Seq[Any]]).recoverWith({ case e: ClassCastException => emptyFailure(value, e) })
  }

  object ValueWasNotAProduct extends Throwable
  def extractProduct(value: Any, grammar: BiGrammar): Try[core.grammar.~[Any, Any]] = value match {
    case ~(left, right) => Try(core.grammar.~(left, right))
    case MissingValue => Try(core.grammar.~(MissingValue, MissingValue))
    case _ => emptyFailure(value, ValueWasNotAProduct, grammar)
  }
}

trait PrintError extends Throwable
{
  override def toString = toDocument.renderString()
  def toDocument: ResponsiveDocument
  val value: Any
  val grammar: BiGrammar
  def partial: ResponsiveDocument
  val depth: Int
  def mapPartial(f: ResponsiveDocument => ResponsiveDocument): PrintError
}

case class NestedError(value: Any, grammar: BiGrammar, inner: PrintError) extends PrintError
{
  def partial = inner.partial
  def mapPartial(f: ResponsiveDocument => ResponsiveDocument) = NestedError(value, grammar, inner.mapPartial(f))
  val depth = inner.depth

  override def toDocument = ("Nested:": ResponsiveDocument) % (
    ("Value:": ResponsiveDocument) ~~ value.toString %
    ("Grammar:": ResponsiveDocument) ~~ grammar.toString %
    inner.toDocument)
}

case class RootError(depth: Int, partial: ResponsiveDocument, value: Any, grammar: BiGrammar, inner: Throwable) extends PrintError {

  def toDocument: ResponsiveDocument = ("print error root": ResponsiveDocument) %
    (s"inner = $inner": ResponsiveDocument) %
    s"value = $value" %
    s"grammar = $grammar" %
    "partial = " % partial.indent(4) %
    (s"depth = $depth": ResponsiveDocument) %
    s"trace = " % inner.getStackTrace.map(e => e.toString: ResponsiveDocument).reduce((a, b) => a % b).indent(4)

  override def mapPartial(f: (ResponsiveDocument) => ResponsiveDocument): PrintError = new RootError(1 + depth, f(partial), value, grammar, inner)
}

class NonePrintFailureException(e: Throwable) extends RuntimeException
{
  override def toString = "failed toDocument with something different than a print failure: " + e.toString
}

object ToDocumentApplicative {

  def or(first: Try[ResponsiveDocument], second: Try[ResponsiveDocument]) : Try[ResponsiveDocument] = {
    first.recoverWith({ case leftFailure: PrintError =>
      second.recoverWith({ case rightFailure: PrintError =>
        combineOrFailures(leftFailure, rightFailure)
      })
    })
  }

  private def combineOrFailures(left: PrintError, right: PrintError): Try[ResponsiveDocument] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)

  def bind(first: Try[ResponsiveDocument => ResponsiveDocument], second: Try[ResponsiveDocument]) : Try[ResponsiveDocument] = {
    first match {
      case Success(f) => second match {
        case Success(secondSuccess) => Success(f(secondSuccess))
        case Failure(printError: PrintError) => Failure(printError.mapPartial(f))
        case Failure(e: NonePrintFailureException) => throw e
        case Failure(e: Throwable) => throw new NonePrintFailureException(e)
      }
      case failure: Failure[_] => Failure(failure.exception)
    }
  }
}