package application.graphing

import application.graphing.TransformationVertex._
import core.transformation.{ProgramTransformation, TransformationManager}
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import transformations.javac.JavaCompiler

import scala.collection.mutable

object TransformationGraph {

  def getGraph: DirectedGraph[TransformationVertex, DefaultEdge] = {
    val transformations = TransformationManager.javaTransformations ++ JavaCompiler.javaCompilerTransformations.toSet
    val result = new DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge])

    depthFirstTraversal[ProgramTransformation](transformations, transformation => transformation.dependencies,
      transformation => result.addVertex(new TransformationVertex(transformation)),
      transformation => {
        for (outgoing <- transformation.dependencies)
          result.addEdge(outgoing, transformation)
      })

    result
  }

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