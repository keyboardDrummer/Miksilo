package miksilo.modularLanguages.core.bigrammar

import miksilo.modularLanguages.core.bigrammar.grammars._
import miksilo.editorParser.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter, SingleResultParser}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter}
import miksilo.languageServer.core.textMate.TextMateGeneratingParserWriter
import miksilo.languageServer.util.Utility

import scala.collection.mutable

case class WithMap[+T](value: T, namedValues: Map[Any,Any] = Map.empty) {}

//noinspection ZeroIndexToHead
object BiGrammarToParser extends CommonParserWriter with LeftRecursiveCorrectingParserWriter
  with IndentationSensitiveParserWriter with TextMateGeneratingParserWriter {

  type AnyWithMap = WithMap[Any]
  type Result = AnyWithMap
  type State = MyState

  case class MyState(state: Map[Any, Any]) extends HasIndentation {
    override def indentation = state.getOrElse(IndentationKey, 0).asInstanceOf[Int]

    override def withIndentation(newIndentation: Int) = MyState(state + (IndentationKey -> newIndentation))
  }

  object IndentationKey {
    override def toString = "Indentation"
  }

  override def startState = MyState(Map.empty)

  def valueToResult(value: Any): Result = WithMap(value, Map.empty)

  def toParser(grammar: BiGrammar): SingleResultParser[Any] = {
    toParserBuilder(grammar).getWholeInputParser()
  }

  def toParserBuilder(grammar: BiGrammar): Parser[Any] = {

    var keywords: Set[String] = Set.empty
    val allGrammars: Set[BiGrammar] = grammar.selfAndDescendants.toSet
    keywords ++= allGrammars.flatMap({
      case keyword: Keyword => if (keyword.reserved) Set(keyword.value) else Set.empty[String]
      case _ => Set.empty[String]
    })

    toParserBuilder(grammar, keywords)
  }

  def toParserBuilder(grammar: BiGrammar, keywords: scala.collection.Set[String]): Parser[Any] = {
    val cache = new mutable.HashMap[BiGrammar, Parser[Result]]
    lazy val recursive: BiGrammar => Parser[Result] = grammar => {
      cache.getOrElseUpdate(grammar, toParser(keywords, recursive, grammar))
    }
    val resultParser = recursive(grammar)
    resultParser.map(executeResult)
  }

  private def executeResult(result: Result): Any = {
    result.value
  }

  def mergeNamedValues(key: Any, first: Any, second: Any): Any = {
    key match {
      case canMerge: CanMerge =>
        canMerge.merge(first, second)
      case _ => first
    }
  }

  trait CanMerge {
    def merge(first: Any, second: Any): Any
  }

  private def toParser(keywords: scala.collection.Set[String], recursive: BiGrammar => Parser[Result], grammar: BiGrammar): Parser[Result] = {
    grammar match {
      case sequence: BiSequence =>
        val firstParser = recursive(sequence.first)
        val secondParser = recursive(sequence.second)
        val parser = leftRightSimple(firstParser, secondParser, (firstResult: Result, secondResult: Result) => {
          val resultValue = sequence.bijective.construct(firstResult.value, secondResult.value)
          val resultMap = Utility.mergeMaps(firstResult.namedValues, secondResult.namedValues, mergeNamedValues)
          WithMap[Any](resultValue, resultMap)
        })
        parser
      case choice: BiChoice =>
        val firstParser = recursive(choice.left)
        val secondParser = recursive(choice.right)
        firstParser | secondParser

      case custom: CustomGrammarWithoutChildren =>
        custom.getParserBuilder(keywords).map(valueToResult)
      case custom: CustomGrammar =>
        custom.toParser(recursive)

      case many: miksilo.modularLanguages.core.bigrammar.grammars.Many =>
        val innerParser = recursive(many.inner)
        val parser = innerParser.many[WithMap[List[Any]]](
          WithMap(List.empty[Any], Map.empty[Any, Any]),
          (element, result) => WithMap(element.value :: result.value, element.namedValues ++ result.namedValues), many.parseGreedy)

        parser
      case mapGrammar: MapGrammar[_, _] =>
        val innerParser = recursive(mapGrammar.inner)
        FilterMap(innerParser, mapGrammar.asInstanceOf[MapGrammar[AnyWithMap, AnyWithMap]].construct)

      case BiFailure(message) => Fail(None, message, History.failPenalty)
      case Print(_) => succeed(()).map(valueToResult)
      case ValueGrammar(value) => succeed(value).map(valueToResult)

      case labelled: Labelled =>
        lazy val inner = recursive(labelled.inner)
        new Lazy(inner, labelled.name) //Laziness to prevent infinite recursion
    }
  }
}