package miksilo.editorParser.parsers.core

import scala.collection.mutable

object GraphAlgorithms {

  def depthFirst[Node](root: Node,
                       getChildren: Node => List[Node],
                       visit: (Boolean, List[Node]) => Unit,
                       backEdgeFound: List[Node] => Unit): Unit = {
    var todoStack = List(List(root))
    var visited = mutable.HashSet.empty[Node]
    while (todoStack.nonEmpty) {
      val path: List[Node] = todoStack.head
      val element = path.head
      todoStack = todoStack.tail
      if (visited.add(element)) {
        visit(true, path)
        todoStack = getChildren(element).map(c => c :: path) ++ todoStack
      } else {
        visit(false, path)
        val index = path.tail.indexOf(element)
        if (index >= 0)
          backEdgeFound(path.take(index + 1))
      }
    }
  }
}
