package application.graphing

import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import TransformationVertex._
import core.transformation.TransformationManager

object TransformationGraph {

  def getGraph: DirectedGraph[TransformationVertex,DefaultEdge] = {
    val transformations = TransformationManager.javaTransformations
    val result = new DefaultDirectedGraph[TransformationVertex,DefaultEdge](classOf[DefaultEdge])
    for(transformation <- transformations)
    {
      val vertex : TransformationVertex = transformation
      result.addVertex(vertex)
    }
    for(transformation <- transformations)
    {
      for(dependency <- transformation.dependencies)
        result.addEdge(dependency, transformation)
    }
    result
  }
}