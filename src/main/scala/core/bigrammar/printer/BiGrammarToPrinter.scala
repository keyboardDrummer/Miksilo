package core.bigrammar.printer

import java.util.Objects

import core.bigrammar._
import core.document.Empty
import core.grammar.{GrammarToParserConverter, StringLiteral, ~}
import core.responsiveDocument.ResponsiveDocument

import scala.util.parsing.input.CharArrayReader
import scala.util.{Failure, Success, Try}

object BiGrammarToPrinter {
  def toDocument(outerValue: Any, grammar: BiGrammar): ResponsiveDocument = {
    new BiGrammarToPrinter().toDocumentCached(WithMap(outerValue, Map.empty), grammar).get
  }
}

class BiGrammarToPrinter {
  case class LabelWithValue(label: Labelled, value: Any)
  var cache: Map[LabelWithValue, Try[ResponsiveDocument]] = Map.empty
  
  def toDocumentCached(withMap: WithMap, grammar: BiGrammar): Try[ResponsiveDocument] = {
    def nestError(result: Try[ResponsiveDocument]) = result.recoverWith[ResponsiveDocument](
      { case e: PrintError => Failure(NestedError(withMap, grammar, e))})

    val result: Try[ResponsiveDocument] = grammar match {
      case choice:Choice => ToDocumentApplicative.or(toDocumentCached(withMap, choice.left), toDocumentCached(withMap, choice.right))
      case FromGrammarWithToString(StringLiteral, _) => Try("\"" + withMap.value + "\"") //TODO remove this and create a BiStringLiteral
      case FromGrammarWithToString(consume, verifyWhenPrinting) =>
        val string = withMap.value.toString
        if (!verifyWhenPrinting)
          Success(string)
        else {
          val parseResult = new GrammarToParserConverter().convert(consume)(new CharArrayReader(string.toCharArray))
          if (parseResult.successful)
            Success(string)
          else
            fail("From identity grammar could not parse string")
        }
      case Keyword(keyword, _) => Try(keyword)
      case Delimiter(keyword) => Try(keyword)
      case labelled: Labelled => labelToDocument(withMap, labelled)
      case many: ManyHorizontal => foldSequence(withMap, many.inner, (left, right) => left ~ right)
      case many: ManyVertical => foldSequence(withMap, many.inner, (left, right) => left % right)
      case sequence: Sequence => foldProduct(withMap, sequence, (left, right) => left ~ right)
      case topBottom: TopBottom => foldProduct(withMap, topBottom, (topDoc, bottomDoc) => topDoc % bottomDoc)
      case mapGrammar: MapGrammar => mapGrammarToDocument(withMap, mapGrammar)
      case BiFailure(message) => failureToGrammar(message, withMap, grammar)
      case ValueGrammar(producedValue) => produceToDocument(withMap, grammar, producedValue)
      case Print(document) => Try(document)
      case As(inner, key) => if (withMap.state.contains(key)) toDocumentCached(WithMap(withMap.state(key), withMap.state), inner) else Try(Empty)
    }

    nestError(result)
  }

  def failureToGrammar(message: String, value: Any, grammar: BiGrammar): Failure[Nothing] = {
    fail("encountered failure", -10000)
  }

  def mapGrammarToDocument(value: WithMap, mapGrammar: MapGrammar): Try[ResponsiveDocument] = {
    for {
      deconstructedValue <- deconstructValue(value, mapGrammar)
      result <- toDocumentCached(deconstructedValue, mapGrammar.inner).recoverWith
        { case e: PrintError => Failure(e.mapPartial(x => x)) }
    } yield result
  }

  case class FoundDirectRecursionInLabel(name: Labelled) extends Throwable {
    override def toString = s"found direct recursion in label: $name"
  }

  def labelToDocument(withMap: WithMap, labelled: Labelled): Try[ResponsiveDocument] = {
    val key = LabelWithValue(labelled, withMap)
    val maybeCachedValue = cache.get(key)
    maybeCachedValue match {
      case Some(cachedValue) => cachedValue
      case None =>
        val oldCache = cache
        cache += key -> fail(FoundDirectRecursionInLabel(labelled), -1000)
        val result = toDocumentCached(withMap, labelled.inner)
        cache = oldCache + (key -> result)
        result
    }
  }

  def foldProduct(value: WithMap, grammar: SequenceLike,
                  combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    for {
      ~(firstValue, secondValue) <- extractProduct(value, grammar)
      firstDocument = toDocumentCached(WithMap(firstValue, value.state), grammar.first)
      secondDocument = toDocumentCached(WithMap(secondValue, value.state), grammar.second)
      result <- combineTwo(firstDocument, secondDocument, combine)
    } yield result
  }

  def foldSequence(value: WithMap, inner: BiGrammar,
                   combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    for {
      valueSequence <- extractSequence(value)
      innerDocuments = valueSequence.map(element => toDocumentCached(WithMap(element, value.state), inner))
      result <- innerDocuments.foldRight[Try[ResponsiveDocument]](Success(Empty))((result, element) => combineTwo(result, element, combine))
    } yield result
  }

  case class ProduceWithDifferentValue(expected: Any) {
    override def toString = s"value was not equal to produce value $expected"
  }
  
  def produceToDocument(withMap: WithMap, grammar: BiGrammar, producedValue: Any): Try[ResponsiveDocument] = {
    if (Objects.equals(producedValue, withMap.value)) Try(Empty)
    else fail(ProduceWithDifferentValue(producedValue), -100)
  }

  def deconstructValue(value: WithMap, grammar: MapGrammar): Try[WithMap] = {
    if (grammar.showMap) {
      grammar.deconstruct(value) match {
        case Some(x) => Try(x.asInstanceOf[WithMap])
        case None => fail("could not deconstruct value")
      }
    }
    else {
      grammar.deconstruct(value.value) match {
        case Some(x) => Try(WithMap(x, value.state))
        case None => fail("could not deconstruct value")
      }
    }
  }

  def fail(inner: Any, depth: Int = 0) = Failure(RootError(depth, Empty, inner))

  def combineTwo(first: Try[ResponsiveDocument], second: => Try[ResponsiveDocument],
                 combine: (ResponsiveDocument, ResponsiveDocument) => ResponsiveDocument): Try[ResponsiveDocument] = {
    ToDocumentApplicative.bind(first.map(firstDoc => secondDoc => combine(firstDoc, secondDoc)), second)
  }

  def extractSequence(withMap: WithMap): Try[Seq[Any]] = withMap.value match {
    case sequence: Seq[Any] => Try(sequence)
    case _ => fail(s"value $withMap.value was not a sequence.")
  }

  def extractProduct(withMap: WithMap, grammar: BiGrammar): Try[core.grammar.~[Any, Any]] = withMap.value match {
    case ~(left, right) => Try(core.grammar.~(left, right))
    case UndefinedDestructuringValue => Try(core.grammar.~(UndefinedDestructuringValue, UndefinedDestructuringValue)) //TODO is this really necessary?
    case _ => fail("value was not a product.")
  }
}
