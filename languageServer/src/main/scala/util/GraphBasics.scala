package util

import scala.collection.mutable

object GraphBasics {

  def traverseBreadth[Node](roots: Seq[Node], getChildren: Node => Seq[Node],
                            shouldContinue: Node => Boolean = (n: Node) => true): Seq[Node] = {
    val visited = mutable.Set.empty[Node]
    val queue = mutable.Queue.empty[Node]
    var result = List.empty[Node]
    for(root <- roots)
      queue.enqueue(root)

    while(queue.nonEmpty)
    {
      val value = queue.dequeue()

      if (visited.add(value))
      {
        if (!shouldContinue(value))
          return result

        result ::= value
        for(child <- getChildren(value))
        {
          queue.enqueue(child)
        }
      }
    }
    result.reverse
  }
}
