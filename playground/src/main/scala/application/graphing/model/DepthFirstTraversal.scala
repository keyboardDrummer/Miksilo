package application.graphing.model

import scala.collection.mutable

object DepthFirstTraversal {

  def traverse[T](nodes: Iterable[T], getOutgoing: T => Set[T], enter: T => Unit, leave: T => Unit) {
    val remainingNodes = mutable.Stack[T]()
    remainingNodes.pushAll(nodes)
    var enteredNodes = Set.empty[T]
    var leftNodes = Set.empty[T]

    def doLeave(node: T) {
      leftNodes += node
      leave(node)
    }

    def doEnter(node: T) {
      remainingNodes.push(node)
      enter(node)
      enteredNodes += node

      for (outgoing <- getOutgoing(node))
        remainingNodes.push(outgoing)
    }

    def processNode(node: T) {
      if (enteredNodes.contains(node)) {
        doLeave(node)
      }
      else {
        doEnter(node)
      }
    }

    while (remainingNodes.nonEmpty) {
      val node = remainingNodes.pop()
      if (!leftNodes.contains(node)) {
        processNode(node)
      }
    }
  }
}
