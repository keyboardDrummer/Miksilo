package core.bigrammar

import core.bigrammar.grammars._
import core.grammar.GrammarToParserConverter

import scala.collection.mutable
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

//noinspection ZeroIndexToHead
object BiGrammarToParser extends JavaTokenParsers with PackratParsers {
  type WithMap = WithMapG[Any]
  type State = Map[Any, Any]
  type Result = StateFull[WithMap]

  def valueToResult(value: Any): Result = (state: State) => (state, WithMapG(value, Map.empty))

  def toParser(grammar: BiGrammar): PackratParser[Any] = {
    val cache: mutable.Map[BiGrammar, PackratParser[Result]] = mutable.Map.empty
    lazy val recursive: BiGrammar => PackratParser[Result] = grammar => {
      cache.getOrElseUpdate(grammar, memo(toParser(recursive, grammar)))
    }
    val resultParser = toParser(recursive, grammar)
    val valueParser = resultParser.map(result => result(Map.empty[Any,Any])._2.value)
    valueParser
  }

  def toParser(recursive: BiGrammar => Parser[Result], grammar: BiGrammar): Parser[Result] = {
    grammar match {
      case sequence: Sequence =>
        val firstParser = recursive(sequence.first)
        val secondParser = recursive(sequence.second)
        val parser = for {
          firstResult <- firstParser
          secondResult <- secondParser
        } yield {
          val result: Result = (state: State) => {
            val firstMap = firstResult(state)
            val secondMap = secondResult(firstMap._1)
            val resultValue = core.grammar.~(firstMap._2.value, secondMap._2.value)
            val resultMap = firstMap._2.map ++ secondMap._2.map
            (secondMap._1, WithMapG[Any](resultValue, resultMap)): (State, WithMapG[Any])
          }
          result
        }
        parser
      case choice: Choice =>
        val firstParser = recursive(choice.left)
        val secondParser = recursive(choice.right)
        if (choice.firstBeforeSecond) firstParser | secondParser else firstParser ||| secondParser

      case custom: CustomGrammarWithoutChildren =>
        val parser: GrammarToParserConverter.PackratParser[Any] =
          GrammarToParserConverter.convert(custom.getGrammar).map(valueToResult)
        Parser {
          in: Input => parser(in) match {
            case GrammarToParserConverter.Success(result, next) => Success[Result](result.asInstanceOf[Result], next)
            case GrammarToParserConverter.Failure(msg, next) => Failure(msg, next)
          }
        }
      case custom: CustomGrammar => custom.toParser(recursive)

      case Keyword(keyword, _, _) => literal(keyword).map(valueToResult)
      case Delimiter(keyword) => literal(keyword).map(valueToResult)
      case many: Many =>
        val innerParser = recursive(many.inner)
        val manyInners = innerParser.* //TODO by implementing * ourselves we can get rid of the intermediate List.
        val parser: Parser[Result] = manyInners.map(elements => {
          val result: Result = (initialState: State) => {
            var state = initialState
            var totalMap = Map.empty[Any, Any]
            var totalValue = List.empty[Any]
            elements.foreach(elementStateM => {
              val elementResult = elementStateM(state)
              state = elementResult._1
              totalMap = totalMap ++ elementResult._2.map
              totalValue ::= elementResult._2.value
            })
            (state, WithMapG[Any](totalValue.reverse, totalMap))
          }
          result
        })
        parser
      case mapGrammar: MapGrammarWithMap =>
        val innerParser = recursive(mapGrammar.inner)
        innerParser.map(result => result.map(mapGrammar.construct))

      case BiFailure(message) => failure(message)
      case Print(_) => success(Unit).map(valueToResult)
      case ValueGrammar(value) => success(value).map(valueToResult)
      case As(inner, key) =>
        val innerParser = recursive(inner)
        innerParser.map(result => result.map { case WithMapG(v, state) => WithMapG(inner, state + (key -> v)) })

      case labelled: Labelled =>
        lazy val inner = recursive(labelled.inner)
        Parser { in => inner.apply(in) } //Laziness to prevent infinite recursion
    }
  }
}
