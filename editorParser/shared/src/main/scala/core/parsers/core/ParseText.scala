package core.parsers.core

import core.parsers.editorParsers.Position

import scala.collection.Searching

final class ParseText extends CharSequence {

  private var _arrayOfChars: Array[Char] = _
  private var lineStarts: Array[Int] = _

  def arrayOfChars: Array[Char] = _arrayOfChars
  def arrayOfChars_=(arrayOfChars: Array[Char]): Unit = {
    _arrayOfChars = arrayOfChars
    lineStarts = {
      var offset = 0
      var result = List(0)
      var index = 0
      while(index < arrayOfChars.length) {
        val char = arrayOfChars(index)
        offset += 1
        if (char == '\n') {
          result ::= offset
        }
        index += 1
      }
      result.reverse.toArray
    }
  }

  def getOffset(position: Position): Int = ???

  def getPosition(offset: Int): Position = {
    import Searching._
    lineStarts.search(offset) match {
      case Found(index) => Position(index, 0)
      case InsertionPoint(insertionPoint) =>
        val line = insertionPoint - 1
        Position(line, offset - lineStarts(line))
    }
  }

  def length: Int                                     = arrayOfChars.length
  def charAt(index: Int): Char                        = arrayOfChars(index)
  def subSequence(start: Int, end: Int): CharSequence = new runtime.ArrayCharSequence(arrayOfChars, start, end)
  override def toString                               = arrayOfChars.mkString
}
