package core.parsers.caching

import core.parsers.core.{Metrics, ParseText, TextPointer}
import core.parsers.editorParsers.{CachingParser, SingleResultParser, StopFunction}
import core.parsers.strings.SubSequence

object ExclusivePointer {
  def getCachingParser[Result](parseText: ParseText, singleResultParser: SingleResultParser[Result]): CachingParser[Result] = {
    val offsetManager = new ArrayOffsetManager(parseText)
    new CachingParser[Result] {

      override def parse(mayStop: StopFunction, metrics: Metrics) = {
        singleResultParser.parse(offsetManager.getOffsetNode(0), mayStop, metrics)
      }

      override def changeRange(from: Int, until: Int, insertionLength: Int): Unit = {
        offsetManager.changeText(from, until, insertionLength)
      }
    }
  }
}

/**
  * A position that includes everything up to but not including a particular offset.
  * This position changes if text is added/removed before the particular offset.
  */
class ExclusivePointer(val manager: ArrayOffsetManager, var offset: Int) extends TextPointer {

  var rightSide: InclusivePointer = new InclusivePointer(this)

  override def cache = rightSide.cache

  override def drop(amount: Int): TextPointer = {
    if (amount == 0)
      rightSide
    else
      manager.getOffsetNode(amount + offset)
  }

  override def charAt(index: Int) = manager.text.charAt(index)

  override def length = manager.text.length

  override def charSequence = new SubSequence(manager.text, offset)

  override def subSequence(from: Int, until: Int) = manager.text.subSequence(from, until)

  override def lineCharacter = manager.text.getPosition(offset)

  override def equals(obj: Any) = {
    throw new Exception("don't compare text pointers, compare their offsets instead.")
  }
}

