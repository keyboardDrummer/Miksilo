package graphing

import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import TransformationVertex._
import transformation.TransformationManager._
object TransformationGraph {

  def getGraph: DirectedGraph[TransformationVertex,DefaultEdge] = {
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