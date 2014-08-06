package application.graphing

import core.transformation.Contract
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import transformations.javac.{JavaCompiler, JavaMinus}

import scala.collection.mutable

class TransformationGraph extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  val transformations: Set[Contract] = Set(JavaMinus) ++ JavaCompiler.javaCompilerTransformations.toSet

  depthFirstTraversal[Contract](transformations, transformation => transformation.dependencies,
    transformation => addVertex(new TransformationVertex(transformation)),
    transformation => {
      for (outgoing <- transformation.dependencies)
        addEdge(outgoing, transformation)
    })

  def depthFirstTraversal[T](nodes: Set[T], getOutgoing: T => Set[T], enter: T => Unit, leave: T => Unit) {
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
