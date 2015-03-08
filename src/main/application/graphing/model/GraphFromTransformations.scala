package application.graphing.model

import core.transformation.Particle
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

class GraphFromTransformations(transformations: Seq[Particle])
  extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  DepthFirstTraversal.traverse[Particle](transformations,
    transformation => transformation.dependencies.collect({case x: Particle => x}),
    transformation => addVertex(new TransformationVertex(transformation)),
    transformation => {
      for (outgoing <- transformation.dependencies)
        addEdge(outgoing, transformation)
    })
}
