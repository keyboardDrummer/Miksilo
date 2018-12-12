package core.bigrammar

import core.bigrammar.BiGrammar.State
import core.bigrammar.grammars._
import core.parsers.strings.{CommonParserWriter, StringReader}

import scala.collection.mutable

case class WithMap[+T](value: T, namedValues: Map[Any,Any] = Map.empty) {
}

//noinspection ZeroIndexToHead
object BiGrammarToParser extends CommonParserWriter {
  type AnyWithMap = WithMap[Any]
  type Result = StateFull[AnyWithMap]

  def valueToResult(value: Any): Result = (state: State) => (state, WithMap(value, Map.empty))

  def toStringParser(grammar: BiGrammar): String => ParseResult[Any] =
    input => toParser(grammar).parseWholeInput(new StringReader(input))

  def toParser(grammar: BiGrammar): EditorParser[Any] = {

    var keywords: Set[String] = Set.empty
    val allGrammars: Set[BiGrammar] = grammar.selfAndDescendants.toSet
    keywords ++= allGrammars.flatMap({
      case keyword: Keyword => if (keyword.reserved) Set(keyword.value) else Set.empty[String]
      case _ => Set.empty[String]
    })

    toParser(grammar, keywords)
  }

  def toParser(grammar: BiGrammar, keywords: scala.collection.Set[String]): EditorParser[Any] = {
    val cache: mutable.Map[BiGrammar, EditorParser[Result]] = mutable.Map.empty
    lazy val recursive: BiGrammar => EditorParser[Result] = grammar => {
      cache.getOrElseUpdate(grammar, toParser(keywords, recursive, grammar))
    }
    val resultParser = toParser(keywords, recursive, grammar)
    resultParser.map(executeResult)
  }

  private def executeResult(result: Result): Any = {
    val afterStateRun = result(Map.empty[Any, Any])
    afterStateRun._2.value
  }

  private def toParser(keywords: scala.collection.Set[String], recursive: BiGrammar => EditorParser[Result], grammar: BiGrammar): EditorParser[Result] = {
    grammar match {
      case sequence: BiSequence =>
        val firstParser = recursive(sequence.first)
        val secondParser = recursive(sequence.second)
        val parser = leftRight(firstParser, secondParser, (firstResult: Result, secondResult: Result) => {
          for {
            first <- firstResult
            second <- secondResult
          } yield {
            val resultValue = sequence.bijective.construct(first.value, second.value)
            val resultMap = first.namedValues ++ second.namedValues
            WithMap[Any](resultValue, resultMap)
          }
        })
        parser
      case choice: Choice =>
        val firstParser = recursive(choice.left)
        val secondParser = recursive(choice.right)
        firstParser | secondParser

      case custom: CustomGrammarWithoutChildren => custom.getParser(keywords).map(valueToResult)
      case custom: CustomGrammar => custom.toParser(recursive)

      case many: core.bigrammar.grammars.Many =>
        val innerParser = recursive(many.inner)
        val parser = innerParser.many[StateFull[WithMap[List[Any]]]](
          StateFull.value(WithMap(List.empty[Any], Map.empty[Any, Any])),
          (element, result) => element.flatMap(w => result.map(w2 => WithMap(w.value :: w2.value, w.namedValues ++ w2.namedValues))))

        parser
      case mapGrammar: MapGrammarWithMap =>
        val innerParser = recursive(mapGrammar.inner)
        innerParser.map(result => result.map(mapGrammar.construct))

      case BiFailure(message) => fail(message)
      case Print(_) => succeed(Unit).map(valueToResult)
      case ValueGrammar(value) => succeed(value).map(valueToResult)

      case labelled: Labelled =>
        lazy val inner = recursive(labelled.inner)
        new EditorLazy(inner) //Laziness to prevent infinite recursion
    }
  }
}
