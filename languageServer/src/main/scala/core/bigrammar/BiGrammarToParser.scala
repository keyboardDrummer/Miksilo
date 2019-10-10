package core.bigrammar

import core.bigrammar.BiGrammar.State
import core.bigrammar.grammars._
import core.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter}
import core.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter}
import languageServer.Position
import util.Utility

import scala.collection.mutable

case class WithMap[+T](value: T, namedValues: Map[Any,Any] = Map.empty) {}


//noinspection ZeroIndexToHead
object BiGrammarToParser extends CommonParserWriter with LeftRecursiveCorrectingParserWriter
  with IndentationSensitiveParserWriter {

  def ignoreLeft[Value] = new SequenceBijective[WithMap[Unit], WithMap[Value], WithMap[Value]]((firstResult, secondResult) => {
    val resultMap = Utility.mergeMaps(firstResult.namedValues, secondResult.namedValues, mergeNamedValues)
    WithMap(secondResult.value, resultMap)
  }, right => Some(WithMap(Unit, right.namedValues), right))

  def ignoreRight[Value] = new SequenceBijective[WithMap[Value], WithMap[Unit], WithMap[Value]]((firstResult, secondResult) => {
    val resultMap = Utility.mergeMaps(firstResult.namedValues, secondResult.namedValues, mergeNamedValues)
    WithMap(firstResult.value, resultMap)
  },left => Some(left, WithMap(Unit, left.namedValues)))

  type AnyWithMap = WithMap[Any]
  type Result = AnyWithMap
  type Input = Reader

  object IndentationKey
  class Reader(array: ArrayCharSequence, offset: Int, position: Position, val state: State)
    extends StringReaderBase(array, offset, position)
    with IndentationReaderLike {

    def withState(newState: State): Reader = new Reader(array, offset, position, newState)

    def this(text: String) {
      this(text.toCharArray, 0, Position(0, 0), Map.empty)
    }

    override def drop(amount: Int) = new Reader(array, offset + amount, move(amount), state)

    override def hashCode(): Int = offset

    override def equals(obj: Any): Boolean = obj match {
      case other: Reader => offset == other.offset && state.equals(other.state)
      case _ => false
    }

    override def indentation = state.getOrElse(IndentationKey, 0).asInstanceOf[Int]

    override def withIndentation(value: Int) = withState(state + (IndentationKey -> value))
  }

  def valueToResult(value: Any): Result = WithMap(value, Map.empty)

  def toParser[Value](grammar: BiGrammar[WithMap[Value]]): SingleResultParser[Value] = {
    toParserBuilder(grammar).getWholeInputParser
  }

  def toParserBuilder[Value](grammar: BiGrammar[WithMap[Value]]): Self[Value] = {

    var keywords: Set[String] = Set.empty
    val allGrammars: Set[BiGrammar[_]] = grammar.selfAndDescendants.toSet
    keywords ++= allGrammars.flatMap({
      case keyword: Keyword => if (keyword.reserved) Set(keyword.value) else Set.empty[String]
      case _ => Set.empty[String]
    })

    toParserBuilderInner(grammar, keywords).map(w => w.value)
  }

  trait Rec {
    def apply[Value](grammar: BiGrammar[Value]): Self[Value]
  }

  def toParserBuilderInner[Value](grammar: BiGrammar[Value], keywords: scala.collection.Set[String]): Self[Value] = {
    val cache: mutable.Map[BiGrammar[_], Self[_]] = mutable.Map.empty
    lazy val recursive: Rec = new Rec {

      override def apply[OtherValue](grammar: BiGrammar[OtherValue]) = {
        lazy val value = toParser(keywords, recursive, grammar)
        cache.getOrElseUpdate(grammar, value)
        value
      }

    }
    recursive(grammar)
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
  private def toParser[Value](keywords: scala.collection.Set[String],
                              recursive: Rec,
                              grammar: BiGrammar[Value]): Self[Value] = {
    grammar match {
      case sequence: BiSequence[_, _, Value] =>
        val firstParser = recursive(sequence.first)
        val secondParser = recursive(sequence.second)
        leftRightSimple(firstParser, secondParser, sequence.bijective.construct)
      case choice: BiChoice[Value] =>
        val firstParser = recursive(choice.left)
        val secondParser = recursive(choice.right)
        firstParser | secondParser

      case custom: CustomGrammarWithoutChildren[Value] =>
        custom.getParserBuilder(keywords).asInstanceOf[Self[Value]]
      case custom: CustomGrammar[Value] =>
        custom.toParser(recursive)

      case many: core.bigrammar.grammars.Many[_] =>
        val innerParser = recursive(many.inner)
        innerParser.*.asInstanceOf[Self[Value]]
      case mapGrammar: MapGrammar[_, Value] =>
        val innerParser = recursive(mapGrammar.inner)
        FilterMap(innerParser, mapGrammar.construct)

      case BiFailure(message) => Fail(None, message, History.failPenalty)
      case Print(_) => succeed(Unit).asInstanceOf[Self[Value]]
      case ValueGrammar(value) => succeed(value)

      case labelled: Labelled[Value] =>
        lazy val inner = recursive(labelled.inner)
        new Lazy[Value](inner.asInstanceOf[Self[Value]], labelled.name) //Laziness to prevent infinite recursion
    }
  }
}