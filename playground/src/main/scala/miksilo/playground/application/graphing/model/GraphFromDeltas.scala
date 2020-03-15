package miksilo.playground.application.graphing.model

import miksilo.modularLanguages.core.deltas.Delta
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

class GraphFromDeltas(deltas: Set[Delta])
  extends DefaultDirectedGraph[DeltaVertex, DefaultEdge](classOf[DefaultEdge]) {

  DepthFirstTraversal.traverse[Delta](deltas,
    delta => delta.dependencies.collect({case x: Delta => x}),
    delta => addVertex(new DeltaVertex(delta)),
    delta => {
      for (outgoing <- delta.dependencies)
        addEdge(outgoing, delta)
    })
}
