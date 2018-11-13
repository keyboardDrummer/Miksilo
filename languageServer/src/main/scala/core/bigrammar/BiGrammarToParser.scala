package core.bigrammar

import core.bigrammar.BiGrammar.State
import core.bigrammar.grammars._
import core.parsers._
import core.parsers.strings.StringReader

import scala.collection.mutable

case class WithMap[+T](value: T, map: Map[Any,Any]) {}

//noinspection ZeroIndexToHead
object BiGrammarToParser extends CommonParserWriter {
  type AnyWithMap = WithMap[Any]
  type Result = StateFull[AnyWithMap]

  def valueToResult(value: Any): Result = (state: State) => (state, WithMap(value, Map.empty))

  def toStringParser(grammar: BiGrammar): String => ParseResult[Any] =
    input => toParser(grammar).parseWholeInput(new StringReader(input))

  def toParser(grammar: BiGrammar): Parser[Any] = {

    var keywords: Set[String] = Set.empty
    val allGrammars: Set[BiGrammar] = grammar.selfAndDescendants.toSet
    keywords ++= allGrammars.flatMap({
      case keyword: Keyword => if (keyword.reserved) Set(keyword.value) else Set.empty[String]
      case _ => Set.empty[String]
    })

    toParser(grammar, keywords)
  }

  def toParser(grammar: BiGrammar, keywords: scala.collection.Set[String]): Parser[Any] = {
    val cache: mutable.Map[BiGrammar, Parser[Result]] = mutable.Map.empty
    lazy val recursive: BiGrammar => Parser[Result] = grammar => {
      cache.getOrElseUpdate(grammar, toParser(keywords, recursive, grammar))
    }
    val resultParser = toParser(keywords, recursive, grammar)
    val valueParser = resultParser.map(result => result(Map.empty[Any, Any])._2.value)
    valueParser
  }

  private def toParser(keywords: scala.collection.Set[String], recursive: BiGrammar => Parser[Result], grammar: BiGrammar): Parser[Result] = {
    grammar match {
      case sequence: core.bigrammar.grammars.Sequence =>
        val firstParser = recursive(sequence.first)
        val secondParser = recursive(sequence.second)
        val parser = new core.parsers.Sequence(firstParser, secondParser, (firstResult: Result, secondResult: Result) => {
          val result: Result = (state: State) => {
            val firstMap = firstResult(state)
            val secondMap = secondResult(firstMap._1)
            val resultValue = (firstMap._2.value, secondMap._2.value)
            val resultMap = firstMap._2.map ++ secondMap._2.map
            (secondMap._1, WithMap[Any](resultValue, resultMap)): (State, WithMap[Any])
          }
          result
        })
        parser
      case choice: Choice =>
        val firstParser = recursive(choice.left)
        val secondParser = recursive(choice.right)
        if (choice.firstBeforeSecond) firstParser | secondParser
        else firstParser ||| secondParser

      case custom: CustomGrammarWithoutChildren => custom.getParser(keywords).map(valueToResult)
      case custom: CustomGrammar => custom.toParser(recursive)

      case many: core.bigrammar.grammars.Many =>
        val innerParser = recursive(many.inner)
        val parser = innerParser.many[StateFull[WithMap[List[Any]]]](
          StateFull.value(WithMap(List.empty[Any], Map.empty[Any, Any])),
          (element, result) => element.flatMap(w => result.map(w2 => WithMap(w.value :: w2.value, w.map ++ w2.map))))

        parser
      case mapGrammar: MapGrammarWithMap =>
        val innerParser = recursive(mapGrammar.inner)
        innerParser.map(result => result.map(mapGrammar.construct))

      case BiFailure(message) => fail(message)
      case Print(_) => succeed(Unit).map(valueToResult)
      case ValueGrammar(value) => succeed(value).map(valueToResult)

      case labelled: Labelled =>
        lazy val inner = recursive(labelled.inner)
        new Lazy(inner) //Laziness to prevent infinite recursion
    }
  }
}
