package util

import scala.collection.mutable

abstract class DataFlowAnalysis[Node, State] {
  def getOutgoingNodes(node: Node): Set[Node]

  def updateState(state: State, node: Node): State

  //Returns None if both states are equal.
  def combineState(first: State, second: State): Option[State]

  val states = mutable.Map[Node, State]()
  val nodeQueue = mutable.Queue[Node]()

  def addRootNode(rootNode: Node, rootState: State): Unit = {
    states.put(rootNode, rootState)
    nodeQueue.enqueue(rootNode)
  }

  def run(rootNode: Node, rootState: State): Map[Node, State] = {
    addRootNode(rootNode, rootState)
    run()
  }

  def run(): Map[Node, State] = {
    while (nodeQueue.nonEmpty) {
      val node = nodeQueue.dequeue()
      val ingoingState = states(node)
      val outgoingState = updateState(ingoingState, node)
      val outgoingNodes = getOutgoingNodes(node)
      for (outgoingNode <- outgoingNodes) {
        val oldOutgoingState = states.get(outgoingNode)
        val optionalNewState = oldOutgoingState.fold[Option[State]](Some(outgoingState))(existingState => {
          combineState(existingState, outgoingState)
        })
        optionalNewState.foreach(newState => {
          states(outgoingNode) = newState
          nodeQueue.enqueue(outgoingNode)
        })
      }
    }
    states.toMap
  }
}
