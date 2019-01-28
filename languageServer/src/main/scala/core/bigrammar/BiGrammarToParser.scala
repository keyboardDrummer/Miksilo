package core.bigrammar

import core.bigrammar.BiGrammar.State
import core.bigrammar.grammars._
import core.parsers.editorParsers.UnambiguousEditorParserWriter
import core.parsers.strings.CommonParserWriter

import scala.collection.mutable
import scala.util.parsing.input.OffsetPosition

case class WithMap[+T](value: T, namedValues: Map[Any,Any] = Map.empty) {}


//noinspection ZeroIndexToHead
object BiGrammarToParser extends CommonParserWriter with UnambiguousEditorParserWriter  {
  type AnyWithMap = WithMap[Any]
  type Result = AnyWithMap
  type Input = Reader

  class Reader(val array: ArrayCharSequence, val offset: Int, val state: State) extends StringReaderLike {

    def this(text: String) {
      this(text.toCharArray, 0, Map.empty)
    }

    lazy val position = OffsetPosition(array, offset)

    override def drop(amount: Int) = new Reader(array, offset + amount, state)

    override def head = array.charAt(offset)

    override def tail = drop(1)

    override def atEnd = offset == array.length

    override def hashCode(): Int = offset

    override def equals(obj: Any): Boolean = obj match {
      case other: Reader => offset == other.offset
      case _ => false
    }

    override def toString: String = {
      array.subSequence(Math.max(0, offset - 10), offset) + " | " + array.subSequence(offset, Math.min(array.length, offset + 10))
    }
  }

  def valueToResult(value: Any): Result = WithMap(value, Map.empty)

  def toStringParser(grammar: BiGrammar): String => ParseResult[Any] =
    input => toParser(grammar).parseWholeInput(new Reader(input))

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
    result.value
  }

  private def toParser(keywords: scala.collection.Set[String], recursive: BiGrammar => EditorParser[Result], grammar: BiGrammar): EditorParser[Result] = {
    grammar match {
      case sequence: BiSequence =>
        val firstParser = recursive(sequence.first)
        val secondParser = recursive(sequence.second)
        val parser = leftRight(firstParser, secondParser, (first: Result, second: Result) => {
          val resultValue = sequence.bijective.construct(first.value, second.value)
          val resultMap = first.namedValues ++ second.namedValues
          WithMap[Any](resultValue, resultMap)
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
        val parser = innerParser.many[WithMap[List[Any]]](
          WithMap(List.empty[Any], Map.empty[Any, Any]),
          (w, w2) => WithMap(w.value :: w2.value, w2.namedValues ++ w.namedValues))

        parser
      case mapGrammar: MapGrammarWithMap =>
        val innerParser = recursive(mapGrammar.inner)
        innerParser.map(mapGrammar.construct)

      case BiFailure(message) => fail(message)
      case Print(_) => succeed(Unit).map(valueToResult)
      case ValueGrammar(value) => succeed(value).map(valueToResult)

      case labelled: Labelled =>
        lazy val inner = recursive(labelled.inner)
        new EditorLazy(inner) //Laziness to prevent infinite recursion
    }
  }
}
