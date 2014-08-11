package application.graphing.model

import application.graphing.model.simplifications._
import com.google.common.collect.Lists
import core.transformation.Contract
import org.jgrapht.alg.StrongConnectivityInspector
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.TopologicalOrderIterator
import transformations.javac.JavaCompiler

import scala.collection.convert.Wrappers
import scala.collection.convert.Wrappers.{JListWrapper, JSetWrapper}

class TransformationGraph extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  val simplifications = Seq(ByteCode, SimpleByteCode, OptimizedByteCode, JavaSimpleExpression
    , JavaSimpleStatement, JavaMethod, JavaC)
  buildInitialGraph()
  addSimplifications()

  //  val sinks: JSetWrapper[TransformationVertex] = getVertices.filter(vertex => this.outDegreeOf(vertex) == 0)
  //  if (sinks.size > 1)
  //    throw new RuntimeException("more than once sink")

  val source: JSetWrapper[TransformationVertex] = getVertices.filter(vertex => this.inDegreeOf(vertex) == 0)
  if (source.size > 1)
    throw new RuntimeException("more than once source")

  optimizeDependencies()

  def buildInitialGraph() {
    val transformations: Set[Contract] = JavaCompiler.javaCompilerTransformations.toSet

    DepthFirstTraversal.traverse[Contract](transformations, transformation => transformation.dependencies,
      transformation => addVertex(new TransformationVertex(transformation)),
      transformation => {
        for (outgoing <- transformation.dependencies)
          addEdge(outgoing, transformation)
      })
  }

  def addSimplifications() {
    for (simplification <- simplifications) {
      addVertex(simplification)

      var dependants = simplification.dependants
      for (dependency <- simplification.dependencies) {
        addVertex(dependency)
        for (derivedDependant <- getOutgoingNodes(dependency)) {
          dependants += derivedDependant.transformation
        }
        addEdge(dependency, simplification)
      }
      for (incoming <- dependants) {
        addVertex(incoming)
        addEdge(simplification, incoming)
      }
    }
  }

  def getOutgoingNodes(vertex: TransformationVertex): Set[TransformationVertex] = {
    new JSetWrapper(this.outgoingEdgesOf(vertex)).map(outgoingEdge => getEdgeTarget(outgoingEdge)).toSet
  }

  def optimizeDependencies() {
    val topologicalOrdering = Lists.newArrayList(new TopologicalOrderIterator(this))

    if (topologicalOrdering.size() < vertexSet().size()) {
      val detector = new StrongConnectivityInspector(this)
      val cycles = JListWrapper(detector.stronglyConnectedSets()).map(s => JSetWrapper(s)).filter(s => s.size > 1)
      if (cycles.nonEmpty) {
        throw new RuntimeException(s"you have cycles: $cycles")
      }

      throw new RuntimeException("topological ordering missed some nodes. ")
    }

    var deepDependencies = Map.empty[TransformationVertex, Map[TransformationVertex, Int]]
    for (vertex <- JListWrapper[TransformationVertex](topologicalOrdering)) {
      deepDependencies += vertex -> Map[TransformationVertex, Int](vertex -> 1)

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

  def getIncomingNodes(dependency: TransformationVertex): Set[TransformationVertex] = {
    JSetWrapper(incomingEdgesOf(dependency)).map(edge => getEdgeSource(edge)).toSet
  }

  def alterNestedMap[K, K2, V](map: Map[K, Map[K2, V]], key: K, key2: K2, default: V, alter: V => V): Map[K, Map[K2, V]] = {
    alterMap(map, key, Map.empty[K2, V], (m: Map[K2, V]) => alterMap(m, key2, default, alter))
  }


  def alterMap[K, V](map: Map[K, V], key: K, default: V, alter: V => V): Map[K, V] = {
    map + (key -> alter(map.getOrElse(key, default)))
  }

  def getVertices: Wrappers.JSetWrapper[TransformationVertex] = {
    JSetWrapper(vertexSet())
  }

}
