package application.graphing.model

import application.graphing.model.simplifications._
import com.google.common.collect.Lists
import core.transformation.Contract
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.TopologicalOrderIterator
import transformations.bytecode.LabelledTargets
import transformations.javac.JavaCompiler

import scala.collection.convert.Wrappers.{JListWrapper, JSetWrapper}

class TransformationGraph extends DefaultDirectedGraph[TransformationVertex, DefaultEdge](classOf[DefaultEdge]) {

  val simplifications = Set(ByteCode, SimpleByteCode, OptimizedByteCode, JavaSimpleExpression, JavaExpression, JavaStatement, JavaC, JavaMethod, JavaClass)
  buildInitialGraph()
  addSimplifications()
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
      addVertex(new TransformationVertex(simplification))
      for (incoming <- simplification.dependants) {
        addVertex(new TransformationVertex(incoming))
        addEdge(simplification, incoming)
      }

      for (outgoing <- simplification.dependencies) {
        addVertex(new TransformationVertex(outgoing))
        addEdge(outgoing, simplification)
      }
    }
  }

  def optimizeDependencies() {
    var deepDependencies = Map.empty[TransformationVertex, Map[Contract, Int]]
    val topologicalOrdering = Lists.newArrayList(new TopologicalOrderIterator(this))
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

    for (vertex <- JSetWrapper(vertexSet())) {
      if (vertex.transformation == LabelledTargets)
        System.out.append(' ')

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

}
