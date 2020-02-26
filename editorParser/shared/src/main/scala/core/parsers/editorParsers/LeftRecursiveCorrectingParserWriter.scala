package core.parsers.editorParsers

import core.parsers.core.{Metrics, OffsetNode, ParseText}
import core.parsers.sequences.SingleResultParser

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.mutable

trait CachingParseResult {
  def latestRemainder: OffsetNode
}

// TODO consider pushing down the caching part into another ParsingWriter
trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  type Input <: CachingInput
  type ParseResult[+Result] <: CachingParseResult
  type CacheKey

  trait CachingInput extends CorrectingInput {
    def offsetNode: CachingOffsetNode
    def createCacheKey(parser: BuiltParser[_], state: Set[BuiltParser[Any]]): CacheKey
  }

  trait CachingOffsetNode extends OffsetNode {
    def drop(amount: Int): CachingOffsetNode
    def cache: mutable.HashMap[CacheKey, ParseResult[_]]
    def cache_=(value: mutable.HashMap[CacheKey, ParseResult[_]]): Unit
  }

  trait OffsetManager {
    def getOffsetNode(offset: Int): CachingOffsetNode
    def changeText(from: Int, until: Int, insertLenght: Int): Unit
    def clear(): Unit
  }

  class AbsoluteOffsetNode(val manager: ArrayOffsetManager, var offset: Int) extends CachingOffsetNode {
    override def getAbsoluteOffset() = offset

    override var cache = new mutable.HashMap[CacheKey, ParseResult[_]]

    override def drop(amount: Int) = manager.getOffsetNode(amount + offset)

    override def toString = offset.toString
  }

  class ArrayOffsetManager extends OffsetManager {
    val offsets = mutable.ArrayBuffer.empty[AbsoluteOffsetNode]
    val offsetCache = mutable.HashMap.empty[Int, CachingOffsetNode]
    override def getOffsetNode(offset: Int) = {
      offsetCache.getOrElseUpdate(offset, {
        binarySearch(offset) match {
          case Found(index) => offsets(index)
          case InsertionPoint(insertionPoint) =>
            val result = new AbsoluteOffsetNode(this, offset)
            offsets.insert(insertionPoint, result)
            result
        }
      })
    }

    @tailrec
    private[this] def binarySearch(offset: Int, from: Int = 0, to: Int = offsets.length): SearchResult = {
      if (to <= from) InsertionPoint(from)
      else {
        val idx = from + (to - from - 1) / 2
        Integer.compare(offset, offsets(idx).getAbsoluteOffset()) match {
          case -1 => binarySearch(offset, from, idx)
          case  1 => binarySearch(offset, idx + 1, to)
          case  _ => Found(idx)
        }
      }
    }

    override def changeText(from: Int, until: Int, insertLength: Int): Unit = {
      offsetCache.clear()

      val delta = insertLength - (until - from)
      for(offset <- offsets.sortBy(o => -o.getAbsoluteOffset())) {
        val absoluteOffset = offset.getAbsoluteOffset()

        val entries = offset.cache.toList
        for(entry <- entries) {
          val entryStart = offset.getAbsoluteOffset()
          val entryEnd = Math.max(entryStart + 1, entry._2.latestRemainder.getAbsoluteOffset())
          val entryIntersectsWithRemoval = from < entryEnd && entryStart < until
          if (entryIntersectsWithRemoval) {
            offset.cache.remove(entry._1)
          }
        }
        if (absoluteOffset > from) {
          offset.offset += delta
        }
        if (absoluteOffset == from) {
          val newNode = getOffsetNode(offset.offset + delta)
          newNode.cache = offset.cache
          offset.cache = new mutable.HashMap[CacheKey, ParseResult[_]]()
        }
      }
    }

    override def clear(): Unit = {
      offsets.clear()
      offsetCache.clear()
    }
  }

  override def newParseState(input: Input) = FixPointState(input.offset, Set.empty)

  def recursionsFor[Result, SeedResult](parseResults: ParseResults[Input, Result], parser: BuiltParser[SeedResult]) = {
    parseResults match {
      case recursiveResults: RecursiveResults[Result] =>
        val remainder = recursiveResults.recursions - parser
        RecursionsList(
          recursiveResults.recursions.getOrElse(parser, List.empty).asInstanceOf[List[RecursiveParseResult[Input, SeedResult, Result]]],
          if (remainder.isEmpty) recursiveResults.tail else RecursiveResults(remainder, recursiveResults.tail))
      case _ => RecursionsList[Input, SeedResult, Result](List.empty, parseResults)
    }
  }

  case class DetectFixPointAndCache[Result](text: ParseText,
                                            parser: BuiltParser[Result])
    extends CheckCache[Result](text, parser) {

    override def apply(input: Input, state: FixPointState): ParseResult[Result] = {
      val newState = moveState(input, state)

      val key = input.createCacheKey(parser, newState.callStack)
      input.offsetNode.cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[ParseResult[Result]]
        case None =>
          getPreviousResult(input, state) match {
            case Some(intermediate) =>
              intermediate

            case None =>
              if (newState.callStack.contains(parser))
                throw new Exception("recursion should have been detected.")
              val nextState = FixPointState(newState.offset, newState.callStack + parser)
              val initialResult = parser(input, nextState)

              val RecursionsList(recursions, resultWithoutRecursion) = recursionsFor(initialResult, parser)
              val foundRecursion = recursions.nonEmpty

              val result = if (foundRecursion)
                grow(recursions, resultWithoutRecursion, initialResult)
              else
                resultWithoutRecursion

              input.offsetNode.cache.put(key, result)
              result
          }
      }
    }

    def grow(recursions: List[RecursiveParseResult[Input, Result, Result]], previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty.empty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = recursions.map((recursive: RecursiveParseResult[Input, Result, Result]) => {
            val results = recursive.get(singleResult(prev))
            results.flatMapReady(
              ready => if (ready.remainder.offset > prev.remainder.offset) singleResult(ready) else SREmpty.empty,
              uniform = false) // TODO maybe set this to uniform = true
          }).reduce((a,b) => a.merge(b))
          grow(recursions, grown, initialResults)
        }
      }, uniform = false)) // The uniform = false here is because applying recursion is similar to a Sequence
    }

    def getPreviousResult(input: Input, state: FixPointState): Option[ParseResult[Result]] = {
      if (state.offset == input.offset && state.callStack.contains(parser))
          Some(RecursiveResults(Map(parser -> List(RecursiveParseResult[Input, Result, Result](x => x))), SREmpty.empty))
      else
        None
    }
  }

  override def wrapParser[Result](text: ParseText,
                                  parser: BuiltParser[Result],
                                  shouldCache: Boolean,
                                  shouldDetectLeftRecursion: Boolean): BuiltParser[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldDetectLeftRecursion) {
        return new DetectFixPointAndCache[Result](text, parser)
      }
      new CheckCache[Result](text, parser)
  }

  // TODO: replace List with something that has constant concat operation.
  case class RecursiveResults[+Result](recursions: Map[BuiltParser[Any], List[RecursiveParseResult[Input, _, Result]]],
                                       tail: ParseResults[Input, Result])
    extends ParseResults[Input, Result] {

    override def nonEmpty = false

    override def pop() = throw new Exception("Can't pop recursions")

    override def toList = tail.toList

    override def tailDepth = 0

    override def merge[Other >: Result](other: ParseResults[Input, Other], depth: Int,
                                        bests: Map[Input, Double] = Map.empty): RecursiveResults[Other] = other match {
      case otherRecursions: RecursiveResults[Result] =>
        val merged = this.recursions.foldLeft(otherRecursions.recursions)((acc, entry) => {
          val value = acc.get(entry._1) match {
            case Some(existingValue) => existingValue ++ entry._2
            case None => entry._2
          }
          acc + (entry._1 -> value)
        })
        RecursiveResults(merged, tail.merge(otherRecursions.tail))
      case _ =>
        RecursiveResults(this.recursions, tail.merge(other))
    }

    override def flatMap[NewResult](f: LazyParseResult[Input, Result] => ParseResults[Input, NewResult], uniform: Boolean) = {
      RecursiveResults(
        recursions.view.mapValues(s => s.map(r => r.compose(pr => pr.flatMap(f, uniform)))).toMap,
        tail.flatMap(f, uniform))
    }

    override def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                                           oldHistory: History[Input]) = {
      if (oldHistory.flawed)
        tail.mapWithHistory(f, oldHistory)
      else
        super.mapWithHistory(f, oldHistory)
    }

    override def latestRemainder = tail.latestRemainder
  }

  def moveState(input: Input, state: FixPointState) = if (state.offset == input.offset) state else FixPointState(input.offset, Set.empty)

  class CheckCache[Result](text: ParseText, parser: BuiltParser[Result]) extends BuiltParser[Result] {
    // TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.

    def apply(input: Input, state: FixPointState): ParseResult[Result] = {
      val newState =  moveState(input, state)
      val key = input.createCacheKey(parser, newState.callStack)

      input.offsetNode.cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[ParseResult[Result]]
        case _ =>
          val value: ParseResult[Result] = parser(input, newState)
          input.offsetNode.cache.put(key, value)
          value
      }
    }

  }

  def startInput(offsetManager: OffsetManager): Input
  def getSingleResultParser[Result](parser: ParserBuilder[Result]): SingleResultParser[Result, Input] = {
    val parserAndCaches = compile(parser).buildParser(parser)
    val offsetManager = new ArrayOffsetManager
    new SingleResultParser[Result, Input] {

      override def parse(text: String, mayStop: StopFunction, metrics: Metrics) = {
        parserAndCaches.text.arrayOfChars = text.toCharArray
        val zero: Input = startInput(offsetManager)
        findBestParseResult(parserAndCaches.text, zero, parserAndCaches.parser, mayStop, metrics)
      }

      override def reset(): Unit = {
        offsetManager.clear()
      }

      override def changeRange(from: Int, until: Int, insertionLength: Int): Unit = {
        offsetManager.changeText(from, until, insertionLength)
      }
    }
  }
}
