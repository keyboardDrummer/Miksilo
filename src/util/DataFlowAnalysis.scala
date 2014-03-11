package util

import scala.collection.mutable

abstract class DataFlowAnalysis[Node, State] {
  def getOutgoingNodes(node: Node) : Set[Node]
  def updateState(state: State, node: Node) : State
  def combineState(first: State, second: State) : State

  def run(rootNode: Node, rootState: State) : Map[Node,State] = {
    val states = mutable.Map(rootNode -> rootState)
    val nodeQueue = mutable.Queue(rootNode)
    
    while(nodeQueue.nonEmpty)
    {
      val node = nodeQueue.dequeue()
      val ingoingState = states(node)
      val outgoingState = updateState(ingoingState, node)
      val outgoingNodes = getOutgoingNodes(node)
      for(outgoingNode <- outgoingNodes)
      {
        val newState = states.get(outgoingNode).fold(outgoingState)(existingState => {
          combineState(existingState, outgoingState)
        })
        states(outgoingNode) = newState
        nodeQueue.enqueue(outgoingNode)
      }
    }

    states.toMap
  }
}
