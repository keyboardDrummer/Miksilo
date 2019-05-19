package core.parsers.editorParsers

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  class CheckCache[Result](parser: Parse[Result]) extends Parse[Result] {

    val cache = mutable.HashMap[Input, ParseResult[Result]]()

    val isCycle = new IsPartOfCycle()
    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      cache.get (input) match {
        case Some(value) =>
          value
        case _ =>
          val newState = if (state.input == input) {
//            if (state.parsers.contains(parser))
//              throw new Exception("recursion should have been detected")
            FixPointState(input, parser :: state.callStack, state.parsers, state.isCycle + (parser -> isCycle))
          } else {
            FixPointState(input, List(parser), Map.empty, Map(parser -> isCycle))
          }
          val value: ParseResult[Result] = parser(input, newState)
          if (!isCycle.partOfCycle && !cache.contains(input)) {
            cache.put(input, value)
          }

          value
      }
    }
  }

  type ParseState = FixPointState

  override def newParseState(input: Input) = FixPointState(input, List.empty, Map.empty, Map.empty)

  trait HasDetectFixPoint[Result] {
    def parser: Parse[Result]

    def getPreviousResult(input: Input, state: ParseState): Option[ParseResult[Result]] = {
      state.parsers.get(parser) match {
        case Some(innerState) if state.input == input =>
          val parts = state.callStack.takeWhile(p => p != parser)
          parts.foreach(part => state.isCycle.get(part) match {
            case Some(find) => find.partOfCycle = true
            case _ =>
          })
          innerState match {
            case found: FoundFixPoint =>
              Some(found.intermediate.asInstanceOf[ParseResult[Result]])
            case find: FindFixPoint =>
              find.foundRecursion = true
              Some(SREmpty)
          }
        case _ => None
      }
    }


    // In plaats van met SortedParseResults te werken, kan ik ook een set van ready results onthouden ?
    // Ik kan onthouden welke ready's ik al gezien heb.
    // Wat voor invloed heeft het als andere fixpoints nieuwe seeds vinden?
    // Dat kan er voor zorgen dat de seed parse verandert, maar niet dat de growths van de oorspronkelijke seed veranderen.

    //During growing, the input can change, and then state.parsers is flushed, and fixpoint detection.
    //Loop detector -> input increase -> CheckCache -> Loop detector -> CheckCache (boem)
    //During the growing, the cache recursion detection fails???

    /*
    Idee: houdt bij welke ready's ik al gezien heb, daar hoef ik niks meer mee te doen.
    Houdt ook een ParseResult[Result] resultaat bij. Hier kan aan gemerged worden als nieuwe ready's gevonden worden.
     */
    def growResult(input: Input, state: ParseState, previous: ReadyParseResult[Result]): ParseResult[Result] = {
      val previousResults = singleResult(previous)
      val newState = FixPointState(input, state.callStack,
        state.parsers + (parser -> FoundFixPoint(previousResults)), state.isCycle)

      val nextResult: ParseResult[Result] = parser(input, newState)
      previousResults.merge(nextResult.flatMapReady(ready => {
        val result = if (!ready.history.flawed && // A faulty grow parsed some space so it was bigger, but it shouldn't have been accepted because it had an error. Need to add a testcase for this.
          ready.remainder.offset > previous.remainder.offset)
          growResult(input, newState, ready)
        else
          SREmpty
        result
      }))
    }
  }

  trait ParserState
  class IsPartOfCycle(var partOfCycle: Boolean = false)
  case class FixPointState(input: Input, callStack: List[Parse[Any]],
                           parsers: Map[Parse[Any], ParserState],
                           isCycle: Map[Parse[Any], IsPartOfCycle])
  class FindFixPoint(var foundRecursion: Boolean = false) extends ParserState
  case class FoundFixPoint(intermediate: ParseResult[Any]) extends ParserState

  class DetectFixPoint[Result](val parser: Parse[Result]) extends HasDetectFixPoint[Result] with Parse[Result] {

    override def apply(input: Input, state: ParseState) = {
      getPreviousResult(input, state) match {
        case None =>

          val detector = new FindFixPoint()
          val newState = if (state.input == input) {
//            if (state.parsers.contains(parser))
//              throw new Exception("")
            FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector), state.isCycle)
          } else {
            FixPointState(input, List(parser), Map(parser -> detector), Map.empty)
          }
          val result = parser(input, newState)
          val grownResult =
            if (detector.foundRecursion)
              result.flatMapReady(ready => growResult(input, newState, ready))
            else
              result
          grownResult
        case Some(result) =>
          result
      }
    }
  }

  class FixPointCache[Result] {

    val seedsSeen: mutable.Set[ReadyParseResult[Result]] = new mutable.HashSet()
    var result: ParseResult[Result] = SREmpty
  }

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends Parse[Result] with HasDetectFixPoint[Result] {

    val cache = new mutable.HashMap[Input, FixPointCache[Result]]

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {

      getPreviousResult(input, state) match {
        case Some(intermediate) =>
          intermediate

        case None =>
          val inputCache = cache.getOrElseUpdate(input, new FixPointCache[Result])

          val detector = new FindFixPoint()
          val newState = if (state.input == input) {
//                if (state.parsers.contains(parser))
//                  throw new Exception("recursion should have been detected.")
            FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector), state.isCycle)
          } else {
            FixPointState(input, List(parser), Map(parser -> detector), Map.empty)
          }
          val seedResult = parser(input, newState).flatMapReady(ready => {
            if (inputCache.seedsSeen.add(ready))
              singleResult(ready)
            else
              SREmpty
          })
          // I believe any left recursion will be detected immediately, if it exists, since the | operator always calls into both children.
          val grownResult =
            if (detector.foundRecursion)
              seedResult.flatMapReady(ready => growResult(input, newState, ready))
            else seedResult

          inputCache.result = inputCache.result.merge(grownResult)

          //Check because an inner fix can run into a recursion of another fix, and therefor stop and not reach the right result.
          //cache.put(input, grownResult)

          inputCache.result
      }
    }

    def growResult(input: Input, inputCache: FixPointCache[Result], state: ParseState, previous: ReadyParseResult[Result]): ParseResult[Result] = {
      val previousResults = singleResult(previous)
      val newState = FixPointState(input, state.callStack,
        state.parsers + (parser -> FoundFixPoint(previousResults)), state.isCycle)

      val nextResult: ParseResult[Result] = parser(input, newState)
      val recursiveCall = nextResult.flatMapReady(ready => {
        val result = if (inputCache.seedsSeen.add(ready) &&
          !ready.history.flawed && // TODO: remove this line | A faulty grow parsed some space so it was bigger, but it shouldn't have been accepted because it had an error. Need to add a testcase for this.
          ready.remainder.offset > previous.remainder.offset)
          growResult(input, newState, ready)
        else
          SREmpty
        result
      })
      previousResults.merge(recursiveCall)
    }
  }

  override def wrapParse[Result](parser: Parse[Result],
                                 shouldCache: Boolean,
                                 shouldDetectLeftRecursion: Boolean): Parse[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldCache && shouldDetectLeftRecursion) {
        return new DetectFixPointAndCache[Result](parser)
      }
      if (shouldCache) {
        return new CheckCache[Result](parser)
      }

      new DetectFixPoint[Result](parser)
  }
}
