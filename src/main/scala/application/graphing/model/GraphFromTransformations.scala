package application.graphing.model

import core.particles.Delta
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

class GraphFromTransformations(transformations: Seq[Delta])
  extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  DepthFirstTraversal.traverse[Delta](transformations,
    transformation => transformation.dependencies.collect({case x: Delta => x}),
    transformation => addVertex(new TransformationVertex(transformation)),
    transformation => {
      for (outgoing <- transformation.dependencies)
        addEdge(outgoing, transformation)
    })
}
