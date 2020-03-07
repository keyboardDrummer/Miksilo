package core.parsers.editorParsers

import core.parsers.core.{ParseText, TextPointer}
import core.parsers.strings.SubSequence

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.mutable

class AbsoluteTextPointer(val manager: ArrayOffsetManager, var offset: Int) extends TextPointer {
  override def getAbsoluteOffset() = offset

  override var cache = new mutable.HashMap[Any, Any]

  override def drop(amount: Int) = manager.getOffsetNode(amount + offset)

  override def toString = offset.toString

  override def charAt(index: Int) = manager.text.charAt(offset)

  override def length = manager.text.length

  override def charSequence = new SubSequence(manager.text, offset)

  override def subSequence(offset: Int) = manager.text.subSequence(this.offset, this.offset + offset)

  override def position = manager.text.getPosition(offset)
}

class ArrayOffsetManager(var text: ParseText) {

  val offsets = mutable.ArrayBuffer.empty[AbsoluteTextPointer]
  val offsetCache = mutable.HashMap.empty[Int, TextPointer]
  def getOffsetNode(offset: Int) = {
    offsetCache.getOrElseUpdate(offset, {
      binarySearch(offset) match {
        case Found(index) => offsets(index)
        case InsertionPoint(insertionPoint) =>
          val result = new AbsoluteTextPointer(this, offset)
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

  def changeText(from: Int, until: Int, insertLength: Int): Unit = {
    offsetCache.clear()

    val delta = insertLength - (until - from)
    for(offset <- offsets.sortBy(o => -o.getAbsoluteOffset())) {
      val absoluteOffset = offset.getAbsoluteOffset()

      val entries = offset.cache.toList
      for(entry <- entries) {
        val entryStart = offset.getAbsoluteOffset()
        val parseResults = entry._2.asInstanceOf[ParseResult[_]]
        val entryEnd = Math.max(entryStart + 1, parseResults.latestRemainder.getAbsoluteOffset())
        val entryIntersectsWithRemoval = from <= entryEnd && entryStart < until
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
        offset.cache = new mutable.HashMap[Any, Any]()
      }
    }
  }

  def clear(): Unit = {
    offsets.clear()
    offsetCache.clear()
  }
}