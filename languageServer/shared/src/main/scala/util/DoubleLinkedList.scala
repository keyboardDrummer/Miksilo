package util

class DoubleLinkedList[T](var value: T) {
  var previous: Option[DoubleLinkedList[T]] = None
  var next: Option[DoubleLinkedList[T]] = None

  def prepend(element: T): Unit = {
    prepend(new DoubleLinkedList[T](value))
  }

  def prepend(node: DoubleLinkedList[T]): Unit = {
    node.previous = Some(this)
    next.foreach(n => {
      n.previous = Some(node)
      node.next = Some(n)
    })
    next = Some(node)
  }

  def remove(): Unit = {
    next.foreach(n => n.previous = previous)
    previous.foreach(p => p.next = next)
    next = None
    previous = None
  }
}
