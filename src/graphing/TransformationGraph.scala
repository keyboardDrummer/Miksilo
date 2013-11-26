package graphing

import languages.{AddIfElse, AddWhile, AddStatementToSSM}
import org.jgrapht.DirectedGraph
import transformation.ProgramTransformation
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import TransformationVertex._

object TransformationGraph {
  val transformations = Seq[ProgramTransformation](AddWhile,AddStatementToSSM, AddIfElse)

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