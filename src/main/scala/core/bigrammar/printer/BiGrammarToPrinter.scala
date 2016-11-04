package core.bigrammar.printer

import java.util.Objects

import core.bigrammar._
import core.document.Empty
import core.grammar.{StringLiteral, ~}
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success, Try}

object BiGrammarToPrinter {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    new BiGrammarToPrinter().toDocumentCached(outerValue, grammar).get
  }
}

class BiGrammarToPrinter {
  case class LabelWithValue(label: Labelled, value: Any)
  var cache: Map[LabelWithValue, Try[ResponsiveDocument]] = Map.empty
  
  def toDocumentCached(value: Any, grammar: BiGrammar): Try[ResponsiveDocument] = {
    def nestError(result: Try[ResponsiveDocument]) = result.recoverWith[ResponsiveDocument](
      { case e: PrintError => Failure(NestedError(value, grammar, e))})

    val result: Try[ResponsiveDocument] = grammar match {
      case choice:Choice => ToDocumentApplicative.or(toDocumentCached(value, choice.left), toDocumentCached(value, choice.right))
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

  def failureToGrammar(value: Any, grammar: BiGrammar): Failure[Nothing] = {
    fail("encountered failure", -10000)
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
        cache += key -> fail(FoundDirectRecursionInLabel(labelled), -1000)
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

  case class ProduceWithDifferentValue(expected: Any) {
    override def toString = s"value was not equal to produce value $expected"
  }
  
  def produceToDocument(value: Any, grammar: BiGrammar, producedValue: Any): Try[ResponsiveDocument] = {
    if (Objects.equals(producedValue, value)) Try(Empty)
    else fail(ProduceWithDifferentValue(producedValue), -100)
  }

  def deconstructValue(value: Any, grammar: MapGrammar): Try[Any] = {
    grammar.deconstruct(value) match {
      case Some(x) => Try(x)
      case None => fail("could not deconstruct value")
    }
  }

  def fail(inner: Any, depth: Int = 0) = Failure(RootError(depth, Empty, inner))

  def combineTwo(first: Try[ResponsiveDocument], second: => Try[ResponsiveDocument],
                 combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    ToDocumentApplicative.bind(first.map(firstDoc => secondDoc => combine(firstDoc, secondDoc)), second)
  }

  def extractSequence(value: Any): Try[Seq[Any]] = value match {
    case sequence: Seq[Any] => Try(sequence)
    case _ => fail(s"value $value was not a sequence.")
  }

  def extractProduct(value: Any, grammar: BiGrammar): Try[core.grammar.~[Any, Any]] = value match {
    case ~(left, right) => Try(core.grammar.~(left, right))
    case UndefinedDestructuringValue => Try(core.grammar.~(UndefinedDestructuringValue, UndefinedDestructuringValue)) //TODO is this really necessary?
    case _ => fail("value was not a product.")
  }
}
