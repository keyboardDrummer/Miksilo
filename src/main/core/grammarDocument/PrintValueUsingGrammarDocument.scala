package core.grammarDocument

import java.util.Objects

import core.document.Empty
import core.grammar.~
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success, Try}

case class PrintFailure(depth: Int, partial: ResponsiveDocument, value: Any, inner: Throwable) extends Throwable {
  override def toString = toDocument.renderString()

  def toDocument: ResponsiveDocument = ("print failure": ResponsiveDocument) %
    ((s"depth = $depth": ResponsiveDocument) % s"value = $value" %
      s"inner = $inner" %
      s"trace = " % inner.getStackTrace.map(e => e.toString: ResponsiveDocument).reduce((a, b) => a % b).indent(4) %
      "partial = " % partial.indent(4)).indent(4)
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

object PrintValueUsingGrammarDocument {
  def emptyFailure(value: Any, inner: Throwable, depth: Int = 0) = Failure(PrintFailure(depth, Empty, value, inner))

  def flatMap(first: Try[ResponsiveDocument], second: => Try[ResponsiveDocument],
              combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    first match {
      case Success(firstSuccess) => second match {
        case Success(secondSuccess) => Success(combine(firstSuccess, secondSuccess))
        case Failure(PrintFailure(depth, partial, value, inner)) =>
          Failure(new PrintFailure(depth + 1, combine(firstSuccess, partial), value, inner))
      }
      case failure: Failure[ResponsiveDocument] => failure
    }
  }

  def deSequence(value: Any) = value match {
    case ~(_, _) => Try(value)
    case MissingValue => Try(core.grammar.~(MissingValue, MissingValue))
    case _ => Failure(PrintFailure(0, Empty, value, ValueWasNotASequence))
  }

  def optionToTry[T](option: Option[T]) = option

  def toDocument(outerValue: Any, grammar: GrammarDocument): ResponsiveDocument = {
    var labelledValues: Map[Labelled, Any] = Map.empty

    def pickBestFailure(left: PrintFailure, right: PrintFailure): Try[ResponsiveDocument] =
      if (left.depth >= right.depth) Failure(left) else Failure(right)

    def sequenceHelper(value: Any, first: GrammarDocument, second: GrammarDocument,
                       combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
      deSequence(value).flatMap(
      { case ~(firstValue, secondValue) => flatMap(helper(firstValue, first), helper(secondValue, second), combine)})
    }

    def manyHelper(value: Any, inner: GrammarDocument, combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument):
    Try[ResponsiveDocument] = {
      for {
        seqValue <- Try.apply(value.asInstanceOf[Seq[Any]]).recoverWith({ case e: ClassCastException => emptyFailure(value, e)})
        innerElements = seqValue.map(element => helper(element, inner))
        result <- innerElements.foldRight[Try[ResponsiveDocument]](Success(Empty))(
          (result, element) => flatMap(result, element, combine))
      } yield result
    }

    def helper(value: Any, grammar: GrammarDocument): Try[ResponsiveDocument] = {
      grammar.simplify match {
        case Sequence(first, second) =>
          sequenceHelper(value, first, second, (left, right) => left ~ right)
        case Choice(first, second) =>
          helper(value, first).recoverWith({ case leftFailure: PrintFailure =>
            helper(value, second).recoverWith({ case rightFailure: PrintFailure =>
              pickBestFailure(leftFailure, rightFailure)
            })
          })
        case Consume(consume) =>
          Try(value.toString)
        case Keyword(keyword) => Try(keyword)
        case Delimiter(keyword) => Try(keyword)
        case labelled: Labelled =>
          if (labelledValues.get(labelled).exists(v => Objects.equals(v, value)))
            return emptyFailure(value, FoundDirectRecursionInLabel(labelled), -1000)
          labelledValues += labelled -> value
          val result = helper(value, labelled.inner)
          labelledValues = labelledValues - labelled //TODO this is incorrect
          result
        case many: ManyHorizontal => manyHelper(value, many.inner, (left, right) => left ~ right)
        case many: ManyVertical => manyHelper(value, many.inner, (left, right) => left % right)
        case MapGrammar(inner, _, deconstruct) => for {
          deconstructedValue <- deconstruct(value).fold[Try[Any]](emptyFailure(value, CouldNotDeconstructValue))(x => Try(x))
          result <- helper(deconstructedValue, inner)
        } yield result
        case TopBottom(top, bottom) =>
          sequenceHelper(value, top, bottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
        case FailureG => emptyFailure(value, EncounteredFailure, -10000)
        case Produce(producedValue) =>
          if (Objects.equals(producedValue, value)) Try(Empty)
          else emptyFailure(value, FoundProduceWithNotEqualValue(producedValue), -100)
        case Print(document) => Try(document)
      }
    }

    helper(outerValue, grammar).get
  }
}
