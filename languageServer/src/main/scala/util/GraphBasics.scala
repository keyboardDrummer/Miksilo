package util

import scala.collection.mutable

object GraphBasics {

  trait NodeAction
  object Continue extends NodeAction
  object Halt extends NodeAction
  object SkipChildren extends NodeAction

  def traverseBreadth[Node](roots: Seq[Node], getChildren: Node => Seq[Node],
                            shouldContinue: Node => NodeAction = (_: Node) => Continue): Seq[Node] = {
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
        shouldContinue(value) match {
          case Halt => return result
          case SkipChildren =>
            result ::= value
          case Continue =>
            result ::= value
            for(child <- getChildren(value))
            {
              queue.enqueue(child)
            }
        }
      }
    }
    result.reverse
  }
}
