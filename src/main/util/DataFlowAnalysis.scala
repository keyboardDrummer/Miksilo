package util

import scala.collection.mutable

abstract class DataFlowAnalysis[Node, State] {
  def getOutgoingNodes(node: Node) : Set[Node]
  def updateState(state: State, node: Node) : State
  def combineState(first: State, second: State) : State

  val states = mutable.Map[Node,State]()
  val nodeQueue = mutable.Queue[Node]()
  def run(rootNode: Node, rootState: State) : Map[Node,State] = {
    states.put(rootNode, rootState)
    nodeQueue.enqueue(rootNode)
    run()
    states.toMap
  }

  def run() {
    while (nodeQueue.nonEmpty) {
      val node = nodeQueue.dequeue()
      val ingoingState = states(node)
      val outgoingState = updateState(ingoingState, node)
      val outgoingNodes = getOutgoingNodes(node)
      for (outgoingNode <- outgoingNodes) {
        val newState = states.get(outgoingNode).fold(outgoingState)(existingState => {
          combineState(existingState, outgoingState)
        })
        states(outgoingNode) = newState
        nodeQueue.enqueue(outgoingNode)
      }
    }
  }
}
