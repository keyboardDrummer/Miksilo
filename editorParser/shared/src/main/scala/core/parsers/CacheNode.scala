//package core.parsers
//
///*
//Probleem, over alle parsers traversen, en voor elke parser alle key-value pairs in de cache bekijken, is te intensief.
//Per parser kunnen we een tree gebruiken om in te cachen.
//Een node heeft als parent een node met een grotere range. Het liefst heeft een node zo min mogelijk children.
//
//Een optie is om elke node de range in twee'en te hakken. Bij expressions verwacht je een 'logische' verdeling van de ranges, waarbij sub-expressions in hun parents genest zijn
//
//Wellicht kunnen we per cache een sorted list van de starts hebben en een sorted list van de ends.
//Het einde van de change moet iig na start van de cache entry liggen, en de start van de change moet iig voor de end van de cache entry liggen.
//
//Change       ----    |      ----- | -------  |  ---
//Cache entry    xxxxx |   xxxxx    |  xxxxx   | xxxxxx
// */
//case class Range(start: Int, end: Int) {
//  def increment(offset: Int): Range = Range(start + offset, end + offset)
//  def contains(range: Range) = range.start >= start && range.end <= end
//}
//
//trait CacheNode {
//  def getRanges(position: Int): List[Range]
//  def insertText(position: Int, length: Int): List[CacheNode]
//  def insertNode(parent: CacheParent): CacheNode = {
//    ???
//  }
//  def tryInsertNode(parent: CacheParent): Option[CacheParent]
//}
//
//case class CacheParent(myOffset: Int, myLength: Int, children: List[CacheParent]) extends CacheNode {
//  def myRange = Range(myOffset, myOffset + myLength)
//
//  override def getRanges(position: Int) = {
//    if (position <= myOffset) {
//      // To the left of this node
//      List.empty
//    } else if (position >= myOffset + myLength) {
//      // To the right of this node
//      List.empty
//    } else {
//      // In this Node
//      val childResults = children.flatMap(child => child.getRanges(position - myOffset).map(r => r.increment(myOffset)))
//      myRange :: childResults
//    }
//  }
//
//  override def insertText(position: Int, length: Int) = {
//    val end = position + length
//    if (end <= myOffset) {
//      // To the left of this node
//      CacheParent(myOffset + length, length, children)
//    } else if (position >= myOffset + myLength) {
//      // To the right of this node
//      this
//    } else {
//      // In this Node
//      val newMe = children match {
//        case Nil => EmptyCache
//        case head :: tail => tail.fold(head)((a,b) => a.tryInsertNode(b))
//      }
//      newMe.insertText(position, length)
//    }
//  }
//
//  override def tryInsertNode(node: CacheParent) = {
//    val containsNode = myRange.contains(node.myRange)
//    if (containsNode) {
//      children.toStream.map(c => c.tryInsertNode(node)).find(c => c.nonEmpty)
//    } else
//      None
//
//  }
//}
//
//object EmptyCache extends CacheNode {
//  override def getRanges(position: Int) = List.empty
//  override def insertText(position: Int, length: Int) = {}
//}
