package playground.application.graphing.model

import playground.application.graphing.model.simplifications._
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import org.jgrapht.alg.{DijkstraShortestPath, StrongConnectivityInspector}
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.traverse.TopologicalOrderIterator

import scala.collection.mutable
import scala.jdk.CollectionConverters

class DeltaGraph
  extends GraphFromDeltas(JavaToByteCodeLanguage.allDeltas) {

  val simplifications = Seq(ByteCodeWithTypes, ByteCode, SimpleByteCode, OptimizedByteCode, JavaSimpleExpression,
    JavaSimpleStatement, JavaMethodGroup, JavaGroup)
  addSimplifications()

  val sources: mutable.Set[DeltaVertex] = getVertices.filter(vertex => this.inDegreeOf(vertex) == 0)
//  if (sources.size > 1)
//    throw new RuntimeException(s"more than once source, sources = $sources.")

  if (sources.isEmpty)
    throw new RuntimeException("zero sources.")

  optimizeDependencies()

  def addSimplifications(): Unit = {
    for (simplification <- simplifications) {
      addVertex(simplification)

      var dependants = simplification.dependants
      for (dependency <- simplification.dependencies) {
        addVertex(dependency)
        addEdge(dependency, simplification)
      }
      for (dependency <- simplification.dependencies) {
        for (derivedDependant <- getOutgoingNodes(dependency)) {
          val dijkstra = DijkstraShortestPath.findPathBetween[DeltaVertex, DefaultEdge](this, derivedDependant, simplification)
          if (dijkstra == null)
            dependants += derivedDependant.contract
        }
      }
      for (incoming <- dependants) {
        addVertex(incoming)
        addEdge(simplification, incoming)
      }
    }
  }

  def getOutgoingNodes(vertex: DeltaVertex): mutable.Set[DeltaVertex] = {
    CollectionConverters.SetHasAsScala(this.outgoingEdgesOf(vertex)).asScala.map(outgoingEdge => getEdgeTarget(outgoingEdge))
  }

  def optimizeDependencies(): Unit = {
    val topologicalOrdering = Array.from(CollectionConverters.IteratorHasAsScala(new TopologicalOrderIterator(this)).asScala)

    if (topologicalOrdering.length < vertexSet().size()) {
      val detector = new StrongConnectivityInspector(this)
      val cycles = CollectionConverters.ListHasAsScala(detector.stronglyConnectedSets()).asScala.filter(s => s.size > 1)
      if (cycles.nonEmpty) {
        throw new RuntimeException(s"you have cycles: $cycles")
      }

      throw new RuntimeException("topological ordering missed some nodes. ")
    }

    var deepDependencies = Map.empty[DeltaVertex, Map[DeltaVertex, Int]]
    for (vertex <- topologicalOrdering) {
      deepDependencies += vertex -> Map[DeltaVertex, Int](vertex -> 1)

      for (outgoingNode <- getIncomingNodes(vertex)) {
        for (dependencyEntry <- deepDependencies(outgoingNode)) {
          val dependency = dependencyEntry._1
          deepDependencies = alterNestedMap(deepDependencies, vertex, dependency, 0, (v: Int) => v + dependencyEntry._2)
        }
      }
    }

    for (vertex <- getVertices) {

      for (directDependency <- getIncomingNodes(vertex)) {
        if (deepDependencies(vertex).getOrElse(directDependency, 0) > 1) {
          removeEdge(directDependency, vertex)
        }
      }
    }
  }

  def getIncomingNodes(dependency: DeltaVertex): mutable.Set[DeltaVertex] = {
    CollectionConverters.SetHasAsScala(incomingEdgesOf(dependency)).asScala.map(edge => getEdgeSource(edge))
  }

  def alterNestedMap[K, K2, V](map: Map[K, Map[K2, V]], key: K, key2: K2, default: V, alter: V => V): Map[K, Map[K2, V]] = {
    alterMap(map, key, Map.empty[K2, V], (m: Map[K2, V]) => alterMap(m, key2, default, alter))
  }


  def alterMap[K, V](map: Map[K, V], key: K, default: V, alter: V => V): Map[K, V] = {
    map + (key -> alter(map.getOrElse(key, default)))
  }

  def getVertices: mutable.Set[DeltaVertex] = {
    CollectionConverters.SetHasAsScala(vertexSet()).asScala
  }
}
