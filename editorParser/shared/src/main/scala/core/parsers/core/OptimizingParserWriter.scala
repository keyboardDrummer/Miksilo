package core.parsers.core

import core.parsers.editorParsers.{Position, SingleResultParser}

import scala.collection.mutable

trait TextPointer {
  def charAt(index: Int): Char
  def length: Int
  def charSequence: CharSequence
  def subSequence(offset: Int): CharSequence
  def position: Position
  def drop(amount: Int): TextPointer
  def getAbsoluteOffset(): Int
  def toPosition(text: ParseText): Position = text.getPosition(getAbsoluteOffset())
}

trait ParseInput {
  def offsetNode: TextPointer
  def offset = offsetNode.getAbsoluteOffset()
}

trait OptimizingParserWriter extends ParserWriter {

  type Input <: ParseInput
  type ParseResult[+Result]
  type Parser[+Result] = ParserBuilder[Result]

  def newParseState(input: Input): FixPointState

  case class FixPointState(offset: Int, // TODO try to remove this offset, since we can also clear the callStack whenever we move forward.
                           callStack: Set[BuiltParser[Any]])

  def wrapParser[Result](parser: BuiltParser[Result],
                         shouldCache: Boolean,
                         shouldDetectLeftRecursion: Boolean): BuiltParser[Result]

  trait BuiltParser[+Result] {
    def apply(input: Input, state: FixPointState): ParseResult[Result]
    def debugName: Any = null
  }

  trait GetParser {
    def apply[Result](parser: Parser[Result]): BuiltParser[Result]
  }

  trait ParserBuilder[+Result] {
    def getParser(recursive: GetParser): BuiltParser[Result]
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

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      lazy val parseOriginal = recursive(original)
      new BuiltParser[Result] {
        override def apply(input: Input, state: FixPointState) = {
          parseOriginal(input, state)
        }

        override def debugName = Lazy.this.debugName

        override def toString = Lazy.this.toString()
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

  case class ParserAndCaches[Result](parser: BuiltParser[Result])

  case class ParserAnalysis(nodesThatShouldCache: Set[ParserBuilder[_]], nodesThatShouldDetectLeftRecursion: Set[ParserBuilder[_]]) {

    def buildParser[Result](root: Parser[Result]): ParserAndCaches[Result] = {
      val cacheOfParses = new mutable.HashMap[Parser[Any], BuiltParser[Any]]
      def recursive: GetParser = new GetParser {
        override def apply[SomeResult](_parser: Parser[SomeResult]): BuiltParser[SomeResult] = {
          cacheOfParses.getOrElseUpdate(_parser, {
            val parser = _parser.asInstanceOf[ParserBuilder[SomeResult]]
            val result = parser.getParser(recursive)
            wrapParser(result, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
          }).asInstanceOf[BuiltParser[SomeResult]]
        }
      }

      val wrappedRoot = recursive(root)
      ParserAndCaches(wrappedRoot)
    }
  }

  class ConsumeCache {
    var values = new mutable.HashMap[ParserBuilder[Any], Boolean]

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

  def getSingleResultParser[Result](text: ParseText, parser: ParserBuilder[Result]): SingleResultParser[Result, Input]

  case class Success[+Result](result: Result, remainder: Input) {
    def map[NewResult](f: Result => NewResult): Success[NewResult] = Success(f(result), remainder)
  }
}
