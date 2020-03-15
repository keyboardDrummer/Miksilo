package miksilo.languageServer.util.cache

import miksilo.languageServer.util.DoubleLinkedList
import scala.collection.mutable

class LeastRecentlyUsedCache[Key, Value](maximumSize: Int) extends Cache[Key, Value] {
  assert(maximumSize > 0, "maximumSize should be bigger than 0")

  class Entry(val key: Key, var value: Value)

  private var lastUsedLink: Option[DoubleLinkedList[Entry]] = None
  private val data = mutable.Map[Key, DoubleLinkedList[Entry]]()

  override def get(node: Key): Option[Value] = {
    data.get(node).map(link => {
      moveToFront(link)
      link.value.value
    })
  }

  override def add(key: Key, value: Value): Unit = {
    data.get(key) match {
      case Some(link) =>
        moveToFront(link)
        link.value.value = value
      case None =>
        val link = new DoubleLinkedList[Entry](new Entry(key, value))
        data.put(key, link)
        addToFront(link)
        prune()
    }
  }

  private def moveToFront(link: DoubleLinkedList[Entry]): Unit = {
    if (lastUsedLink.contains(link))
      return

    link.remove()
    addToFront(link)
  }

  private def addToFront(link: DoubleLinkedList[Entry]): Unit = {
    lastUsedLink.foreach(previousFront => previousFront.prepend(link))
    lastUsedLink = Some(link)
  }

  private def prune(): Unit = {
    if (data.size > maximumSize) {
      val earliestUsedLink = lastUsedLink.get.previous.get
      earliestUsedLink.remove()
      data.remove(earliestUsedLink.value.key)
    }
  }

  override def size: Int = data.size
}
