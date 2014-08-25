package application.graphing.model

import core.transformation.sillyCodePieces.Injector
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

class GraphFromTransformations(transformations: Seq[Injector])
  extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  DepthFirstTraversal.traverse[Injector](transformations,
    transformation => transformation.dependencies.collect({case x: Injector => x}),
    transformation => addVertex(new TransformationVertex(transformation)),
    transformation => {
      for (outgoing <- transformation.dependencies)
        addEdge(outgoing, transformation)
    })
}
