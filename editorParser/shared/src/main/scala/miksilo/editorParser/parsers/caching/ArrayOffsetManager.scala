package miksilo.editorParser.parsers.caching

import miksilo.editorParser.parsers.core.{Metrics, OffsetPointer, ParseText}
import miksilo.editorParser.parsers.editorParsers._

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.mutable

object ArrayOffsetManager {
  def getCachingParser[Result](parseText: ParseText, singleResultParser: SingleResultParser[Result], indentationSensitive: Boolean): CachingParser[Result] = {
    val offsetManager = new ArrayOffsetManager(parseText, indentationSensitive)
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

class ArrayOffsetManager(var text: ParseText, indentationSensitive: Boolean) {

  val offsets = mutable.ArrayBuffer.empty[ExclusivePointer]
  val offsetCache = mutable.HashMap.empty[Int, ExclusivePointer]

  def getOffsetNode(offset: Int): ExclusivePointer = {
    offsetCache.getOrElseUpdate(offset, {
      binarySearch(offset) match {
        case Found(index) => offsets(index)
        case InsertionPoint(insertionPoint) =>
          val result = new ExclusivePointer(this, offset)
          offsets.insert(insertionPoint, result)
          result
      }
    })
  }

  @tailrec
  private[this] def binarySearch[T](offset: Int, from: Int = 0, to: Int = offsets.length): SearchResult = {
    if (to <= from) InsertionPoint(from)
    else {
      val idx = from + (to - from - 1) / 2
      Integer.compare(offset, offsets(idx).offset) match {
        case -1 => binarySearch(offset, from, idx)
        case  1 => binarySearch(offset, idx + 1, to)
        case  _ => Found(idx)
      }
    }
  }

  def changeText(from: Int, until: Int, insertLength: Int): Unit = {
    offsetCache.clear()

    offsets.zipWithIndex.reverse.foreach(t => {
      val (offset, index) = t
      if (from <= offset.offset && offset.offset < until) {
        offsets.remove(index)
      }
    })

    val delta = insertLength - (until - from)
    for(offset <- offsets.sortBy(o => -o.offset)) {
      val absoluteOffset = offset.offset

      val entries = offset.cache.toList
      for(entry <- entries) {
        val entryStart = offset.offset
        val parseResults = entry._2.asInstanceOf[ParseResults[_, _]]
        val entryEnd = Math.max(entryStart + 1, parseResults.latestRemainder.offset)
        val entryIntersectsWithRemoval = from <= entryEnd && entryStart < until
        if (entryIntersectsWithRemoval) {
          offset.cache.remove(entry._1)
        }
      }
      val oldLineCharacter = offset.lineCharacter
      if (indentationSensitive && absoluteOffset >= from) {
        val newPosition = text.getPosition(offset.offset + delta)
        if (newPosition.character != oldLineCharacter.character) {
          offset.cache.clear()
        }
      }
      if (absoluteOffset > from) {
        offset.offset += delta
      }
      offset.lineCharacter = text.getPosition(offset.offset)
      if (absoluteOffset == from) {
        val newLeftSide = getOffsetNode(offset.offset + delta)
        offset.rightSide.leftSide = newLeftSide
        newLeftSide.rightSide = offset.rightSide
        offset.rightSide = new InclusivePointer(offset)
      }
    }
  }

  def clear(): Unit = {
    offsets.clear()
    offsetCache.clear()
  }
}
