package core.parsers.core

import scala.collection.mutable

trait OptimizingParseResult {
  def latestRemainder: OffsetNodeBase
}

trait OffsetNodeBase {
  def getAbsoluteOffset(): Int
}

trait OptimizingParserWriter extends ParserWriter {

  trait ParseInput2 extends ParseInput {
    def offsetNode: OffsetNode
    def drop(amount: Int): Input
    def atEnd(array: ParseText): Boolean
  }

  type Input <: ParseInput2

  type Parser[+Result] = ParserBuilder[Result]
  def newParseState(input: Input): ParseState
  type ParseState = FixPointState
  type ParseResult[+Result] <: OptimizingParseResult

  trait OffsetManager {
    def getOffsetNode(offset: Int): OffsetNode
    def changeText(from: Int, until: Int, insertLenght: Int): Unit
    def clear(): Unit
  }

  trait OffsetNode extends OffsetNodeBase {
    def drop(amount: Int): OffsetNode
    def cache: mutable.HashMap[(BuiltParser[_], Set[BuiltParser[Any]]), ParseResult[_]]
    def cache_=(value: mutable.HashMap[(BuiltParser[_], Set[BuiltParser[Any]]), ParseResult[_]]): Unit
  }

  class AbsoluteOffsetNode(val manager: ArrayOffsetManager, var offset: Int) extends OffsetNode {
    override def getAbsoluteOffset() = offset

    override var cache = new mutable.HashMap[(BuiltParser[_], Set[BuiltParser[Any]]), ParseResult[_]]

    override def drop(amount: Int) = manager.getOffsetNode(amount + offset)

    override def toString = offset.toString
  }

  case class FixPointState(offset: Int, parsers: Set[BuiltParser[Any]])
  class ArrayOffsetManager extends OffsetManager {
    val offsets = mutable.ArrayBuffer.empty[AbsoluteOffsetNode]
    val offsetCache = mutable.HashMap.empty[Int, OffsetNode]
    override def getOffsetNode(offset: Int) = {
      offsetCache.getOrElseUpdate(offset, {
        val existing = offsets.find(o => o.getAbsoluteOffset() == offset)
        existing.getOrElse({
          val result = new AbsoluteOffsetNode(this, offset)
          offsets.addOne(result)
          result
        })
      })
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
          offset.cache = new mutable.HashMap[(BuiltParser[_], Set[BuiltParser[Any]]), ParseResult[_]]()
        }
      }
    }

    override def clear(): Unit = ???
  }

  def wrapParser[Result](text: ParseText,
                         parser: BuiltParser[Result],
                         shouldCache: Boolean,
                         shouldDetectLeftRecursion: Boolean): BuiltParser[Result]

  trait BuiltParser[+Result] {
    def apply(input: Input, state: ParseState): ParseResult[Result]
    def debugName: Any = null
  }

  trait GetParser {
    def apply[Result](parser: Parser[Result]): BuiltParser[Result]
  }

  trait ParserBuilder[+Result] {
    def getParser(offsettext: ParseText, recursive: GetParser): BuiltParser[Result]
    def mustConsumeInput: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[ParserBuilder[_]]
    def children: List[ParserBuilder[_]]
  }

  trait ParserBuilderBase[Result] extends ParserBuilder[Result] {
    var mustConsumeInput: Boolean = false
  }

  trait SequenceLike[+Result] extends ParserBuilder[Result] {
    def left: Parser[Any]
    def right: Parser[Any]

    override def children = List(left, right)

    override def leftChildren: List[ParserBuilder[Any]] = if (left.mustConsumeInput) List(left) else List(left, right)

    override def getMustConsume(cache: ConsumeCache) = cache(left) || cache(right)
  }

  trait ChoiceLike[+Result] extends ParserBuilder[Result] {
    def first: Parser[Result]
    def second: Parser[Result]

    override def children = List(first, second)

    override def leftChildren = List(first, second)

    override def getMustConsume(cache: ConsumeCache) = cache(first) && cache(second)
  }

  trait ParserWrapper[+Result] extends ParserBuilder[Result] {
    def original: ParserBuilder[Any]

    override def getMustConsume(cache: ConsumeCache) = cache(original)

    override def leftChildren = List(original)

    override def children = List(original)
  }

  class Lazy[Result](_original: => Parser[Result], val debugName: Any = null)
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {
    lazy val original: Parser[Result] = _original
    def getOriginal = original

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
      lazy val parseOriginal = recursive(original)
      new BuiltParser[Result] {
        override def apply(input: Input, state: ParseState) = {
          parseOriginal(input, state)
        }

        override def debugName = Lazy.this.debugName

        override def toString = if (debugName != null) debugName.toString else super.toString
      }
    }

    override def toString = if (debugName != null) debugName.toString else super.toString

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  trait LeafParser[+Result] extends ParserBuilder[Result] {
    override def leftChildren = List.empty

    override def children = List.empty
  }

  def compile[Result](root: Parser[Result]): ParserAnalysis = {
    var nodesThatShouldDetectLeftRecursion: Set[ParserBuilder[_]] = Set.empty
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[ParserBuilder[_], mutable.Set[ParserBuilder[_]]]
    GraphAlgorithms.depthFirst[ParserBuilder[_]](root,
      node => {
        node.asInstanceOf[ParserBuilderBase[Any]].mustConsumeInput = mustConsumeCache(node)
        node.children
      },
      (_, path: List[ParserBuilder[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
          nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = StronglyConnectedComponents.computeComponents[ParserBuilder[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[ParserBuilder[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[ParserBuilder[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[ParserBuilder[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[ParserBuilder[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges

    ParserAnalysis(nodesThatShouldCache, nodesThatShouldDetectLeftRecursion)
  }

  case class ParserAndCaches[Result](text: ParseText,
                                     offsetManager: OffsetManager,
                                     parser: BuiltParser[Result])

  case class ParserAnalysis(nodesThatShouldCache: Set[ParserBuilder[_]], nodesThatShouldDetectLeftRecursion: Set[ParserBuilder[_]]) {

    def buildParser[Result](root: Parser[Result]): ParserAndCaches[Result] = {
      val text = new ParseText()
      val offsetManager = new ArrayOffsetManager
      val cacheOfParses = new mutable.HashMap[Parser[Any], BuiltParser[Any]]
      def recursive: GetParser = new GetParser {
        override def apply[SomeResult](_parser: Parser[SomeResult]): BuiltParser[SomeResult] = {
          cacheOfParses.getOrElseUpdate(_parser, {
            val parser = _parser.asInstanceOf[ParserBuilder[SomeResult]]
            val result = parser.getParser(text, recursive)
            wrapParser(text, result, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
          }).asInstanceOf[BuiltParser[SomeResult]]
        }
      }

      val wrappedRoot = recursive(root)
      ParserAndCaches(text, offsetManager, wrappedRoot)
    }
  }

  class ConsumeCache {
    var values = mutable.Map.empty[ParserBuilder[Any], Boolean]

    def apply[Result](parser: ParserBuilder[Any]): Boolean = {
      values.get(parser) match {
        case Some(v) => v
        case None =>
          values.put(parser, false)
          val value = parser.getMustConsume(this)
          values.put(parser, value)
          value
      }
    }
  }

  implicit class ParserExtensions[+Result](parser: Parser[Result]) extends super.ParserExtensions(parser) {

    def addAlternative[Other >: Result](getAlternative: (Parser[Other], Parser[Other]) => Parser[Other], debugName: Any = null): Parser[Other] = {
      lazy val result: Parser[Other] = new Lazy(parser | getAlternative(parser, result), debugName)
      result
    }
  }
}
