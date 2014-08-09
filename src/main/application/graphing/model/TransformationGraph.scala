package application.graphing.model

import application.graphing.model.simplifications._
import com.google.common.collect.Lists
import core.transformation.Contract
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.TopologicalOrderIterator
import transformations.javac.JavaCompiler

import scala.collection.convert.Wrappers
import scala.collection.convert.Wrappers.{JListWrapper, JSetWrapper}

class TransformationGraph extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  val simplifications = Set(ByteCode, SimpleByteCode, OptimizedByteCode, JavaSimpleExpression,
    JavaExpression, JavaStatement, JavaC, JavaMethod, JavaClass, JavaSimpleStatement)
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
      for (incoming <- simplification.dependants) {
        addVertex(incoming)
        addEdge(simplification, incoming)
      }

      for (dependency <- simplification.dependencies) {
        addVertex(dependency)
        //        val derivedDependants: Set[TransformationVertex] = getIncomingNodes(dependency)
        //        for(derivedDependant <- derivedDependants)
        //        {
        //          //removeEdge(derivedDependant, dependency)
        //          addEdge(simplification, derivedDependant)
        //        }
        addEdge(dependency, simplification)
      }

    }
  }

  def getIncomingNodes(dependency: Contract): Set[TransformationVertex] = {
    JSetWrapper(incomingEdgesOf(dependency)).map(edge => getEdgeSource(edge)).toSet
  }

  def optimizeDependencies() {
    var deepDependencies = Map.empty[TransformationVertex, Map[Contract, Int]]
    val topologicalOrdering = Lists.newArrayList(new TopologicalOrderIterator(this))

    if (topologicalOrdering.size() < vertexSet().size())
      throw new RuntimeException

    for (vertex <- JListWrapper[TransformationVertex](topologicalOrdering)) {
      deepDependencies += vertex -> Map[Contract, Int](vertex.transformation -> 1)

      for (outgoingEdge <- new JSetWrapper(this.incomingEdgesOf(vertex))) {
        val outgoingNode = getEdgeSource(outgoingEdge)
        for (dependencyEntry <- deepDependencies(outgoingNode)) {
          val dependency: Contract = dependencyEntry._1
          deepDependencies = alterNestedMap(deepDependencies, vertex, dependency, 0, (v: Int) => v + dependencyEntry._2)
        }
      }
    }

    for (vertex <- getVertices) {

      for (directDependency <- vertex.transformation.dependencies) {
        if (deepDependencies(vertex).getOrElse(directDependency, 0) > 1) {
          removeEdge(directDependency, vertex)
        }
      }
    }
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
