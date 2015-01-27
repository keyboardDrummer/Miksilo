package core.grammarDocument

import java.util.Objects

import core.document.Empty
import core.grammar.~
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success, Try}

case class PrintFailure(depth: Int, partial: ResponsiveDocument, value: Any, inner: Throwable) extends Throwable {
  override def toString = toDocument.renderString()

  def toDocument: ResponsiveDocument = ("print failure": ResponsiveDocument) %
    (s"inner = $inner": ResponsiveDocument) %
    "partial = " % partial.indent(4) %
    (s"depth = $depth": ResponsiveDocument) %
    s"value = $value" %
    s"trace = " % inner.getStackTrace.map(e => e.toString: ResponsiveDocument).reduce((a, b) => a % b).indent(4)
}

object ValueWasNotASequence extends Throwable

object CouldNotDeconstructValue extends Throwable

case class FoundDirectRecursionInLabel(name: Labelled) extends Throwable {
  override def toString = s"found direct recursion in label: $name"
}

case class FoundProduceWithNotEqualValue(expected: Any) extends Throwable {
  override def toString = s"value was not equal to produce value $expected"
}

object EncounteredFailure extends Throwable

object BiGrammarToDocument {

  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    var labelledValues: Map[Labelled, Any] = Map.empty

    def toDocumentCached(value: Any, grammar: BiGrammar): Try[ResponsiveDocument] = grammar match {

      case Sequence(first, second) =>
        foldProduct(value, first, second, (left, right) => left ~ right)
      case Choice(first, second) =>
        toDocumentCached(value, first).recoverWith({ case leftFailure: PrintFailure =>
          toDocumentCached(value, second).recoverWith({ case rightFailure: PrintFailure =>
            pickBestFailure(leftFailure, rightFailure)
          })
        })
      case Consume(consume) => Try(value.toString)
      case Keyword(keyword) => Try(keyword)
      case Delimiter(keyword) => Try(keyword)
      case labelled: Labelled =>
        if (labelledValues.get(labelled).exists(v => Objects.equals(v, value)))
          return emptyFailure(value, FoundDirectRecursionInLabel(labelled), -1000)
        labelledValues += labelled -> value
        val result = toDocumentCached(value, labelled.inner)
        labelledValues = labelledValues - labelled //TODO this is incorrect
        result
      case many: ManyHorizontal => foldSequence(value, many.inner, (left, right) => left ~ right)
      case many: ManyVertical => foldSequence(value, many.inner, (left, right) => left % right)
      case MapGrammar(inner, _, deconstruct) => for {
        deconstructedValue <- deconstruct(value).fold[Try[Any]](emptyFailure(value, CouldNotDeconstructValue))(x => Try(x))
        result <- toDocumentCached(deconstructedValue, inner)
      } yield result
      case TopBottom(top, bottom) =>
        foldProduct(value, top, bottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
      case BiFailure => emptyFailure(value, EncounteredFailure, -10000)
      case Produce(producedValue) =>
        if (Objects.equals(producedValue, value)) Try(Empty)
        else emptyFailure(value, FoundProduceWithNotEqualValue(producedValue), -100)
      case Print(document) => Try(document)
    }

    def foldProduct(value: Any, first: BiGrammar, second: BiGrammar,
                    combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument):
    Try[ResponsiveDocument] = {
      extractProduct(value).flatMap({ case ~(firstValue, secondValue) =>
        flatMap(toDocumentCached(firstValue, first), toDocumentCached(secondValue, second), combine)
      })
    }

    def foldSequence(value: Any, inner: BiGrammar, combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument):
    Try[ResponsiveDocument] = {
      for {
        valueSequence <- Try.apply(value.asInstanceOf[Seq[Any]]).recoverWith({ case e: ClassCastException => emptyFailure(value, e)})
        innerDocuments = valueSequence.map(element => toDocumentCached(element, inner))
        result <- innerDocuments.foldRight[Try[ResponsiveDocument]](Success(Empty))(
          (result, element) => flatMap(result, element, combine))
      } yield result
    }

    toDocumentCached(outerValue, grammar).get
  }

  def pickBestFailure(left: PrintFailure, right: PrintFailure): Try[ResponsiveDocument] =
    if (left.depth >= right.depth) Failure(left) else Failure(right)

  def emptyFailure(value: Any, inner: Throwable, depth: Int = 0) = Failure(PrintFailure(depth, Empty, value, inner))

  def flatMap(first: Try[ResponsiveDocument], second: => Try[ResponsiveDocument],
              combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    first match {
      case Success(firstSuccess) => second match {
        case Success(secondSuccess) => Success(combine(firstSuccess, secondSuccess))
        case Failure(PrintFailure(depth, partial, value, inner)) =>
          Failure(new PrintFailure(depth + 1, combine(firstSuccess, partial), value, inner))
        case Failure(e: NonePrintFailureException) => throw e
        case Failure(e: Throwable) =>
          throw new NonePrintFailureException(e)
      }
      case failure: Failure[ResponsiveDocument] => failure
    }
  }

  class NonePrintFailureException(e: Throwable) extends RuntimeException
  {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def extractProduct(value: Any): Try[core.grammar.~[Any, Any]] = value match {
    case ~(left, right) => Try(core.grammar.~(left, right))
    case MissingValue => Try(core.grammar.~(MissingValue, MissingValue))
    case _ => emptyFailure(value, ValueWasNotASequence)
  }
}
