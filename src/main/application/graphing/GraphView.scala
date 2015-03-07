package application.graphing

import java.util

import application.graphing.model.simplifications.TransformationGroup
import application.graphing.model.{TransformationGraph, TransformationVertex}
import com.google.common.collect.Lists
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.mxCell
import com.mxgraph.util.mxConstants
import com.mxgraph.view.{mxGraph, mxStylesheet}
import core.transformation.sillyCodePieces.{ParticleWithPhase, GrammarTransformation}
import org.jgrapht.traverse.TopologicalOrderIterator

import scala.collection.convert.Wrappers.{JListWrapper, JSetWrapper}

class GraphView(origin: TransformationGraph) extends mxGraph {

  initialise()

  def initialise() {

    this.getModel.beginUpdate()

    setCellsResizable(false)
    setConnectableEdges(false)
    setResetEdgesOnMove(true)

    setStyleSheet()
    var vertexMap: Map[TransformationVertex, mxCell] = Map.empty[TransformationVertex, mxCell]
    val parent = getDefaultParent

    val topologicalOrdering = Lists.reverse(Lists.newArrayList(new TopologicalOrderIterator(origin)))
    for (vertex <- JListWrapper(topologicalOrdering)) {
      val cell: mxCell = getVertex(parent, vertex, vertexMap)
      vertexMap += (vertex -> cell)
    }
    for (edge <- new JSetWrapper(origin.edgeSet())) {
      val source = vertexMap(origin.getEdgeSource(edge))
      val target = vertexMap(origin.getEdgeTarget(edge))
      insertEdge(parent, source, target)
    }

    setLayout()

    this.getModel.endUpdate()
  }


  def setStyleSheet() {
    val stylesheet: mxStylesheet = getStylesheet
    val simplification = new util.Hashtable[String, Object]()
    simplification.put(mxConstants.STYLE_FILLCOLOR, "#FFFFFF")

    val simplificationStyle = new util.Hashtable[String, Object]()
    simplificationStyle.put(mxConstants.STYLE_FILLCOLOR, "#ABC123")

    val grammarStyle = new util.Hashtable[String, Object]()
    grammarStyle.put(mxConstants.STYLE_FILLCOLOR, "#44E35C")

    val transformationStyle = new util.Hashtable[String, Object]()
    transformationStyle.put(mxConstants.STYLE_FILLCOLOR, "#FF6666")

    stylesheet.putCellStyle("GRAMMAR", grammarStyle)
    stylesheet.putCellStyle("SIMPLIFICATION", simplification)
    stylesheet.putCellStyle("TRANSFORMATION", transformationStyle)
  }

  def setLayout() {
    val layout = new mxHierarchicalLayout(this)
    layout.setIntraCellSpacing(layout.getIntraCellSpacing * 1.0)
    layout.setParallelEdgeSpacing(layout.getParallelEdgeSpacing * 1.0)
    layout.setInterRankCellSpacing(layout.getInterRankCellSpacing * 1.5)
    layout.setFineTuning(true)
    layout.execute(getDefaultParent)
    layout.setMoveParent(true)
    layout.setResizeParent(true)
  }

  def insertEdge(parent: AnyRef, source: AnyRef, target: AnyRef): AnyRef = {
    insertEdge(parent, null, "", source, target)
  }

  def getVertex(parent: AnyRef, vertex: TransformationVertex, vertexMap: Map[TransformationVertex, mxCell]): mxCell = {
    val vertexLabel = vertex.toString
    val height: Double = 30
    val width = Math.max(getCellWidthBasedOnLabel(vertexLabel), getCellWidthBasedOnDependencies(vertex, vertexMap))
    val cell = insertVertex(parent, null, "", 20, 20, width, height).asInstanceOf[mxCell]
    cell.setValue(vertexLabel)
    vertex.transformation match {
      case _: TransformationGroup =>
        cell.setStyle("SIMPLIFICATION")

      case _: ParticleWithPhase =>
        cell.setStyle("TRANSFORMATION")

      case _: GrammarTransformation =>
        cell.setStyle("GRAMMAR")

      case _ =>
    }

    cell
  }


  def getCellWidthBasedOnDependencies(vertex: TransformationVertex, vertexMap: Map[TransformationVertex, mxCell]) = {
    val incoming = origin.inDegreeOf(vertex)
    val outgoing = origin.outDegreeOf(vertex)
    val maximum = Math.max(incoming, outgoing)
    maximum * 100
  }

  //
  //  def getCellWidthBasedOnDependencies(vertex: TransformationVertex, vertexMap: Map[TransformationVertex, mxCell]) = {
  //    val incoming = vertex.transformation.dependencies.size
  //    var outgoingWidth = 0.0
  //    val outgoingEdgesOf = origin.outgoingEdgesOf(vertex)
  //    for (outgoing <- JSetWrapper(outgoingEdgesOf)) {
  //      val outgoingCell = vertexMap(origin.getEdgeTarget(outgoing))
  //      outgoingWidth += outgoingCell.getGeometry.getWidth
  //    }
  //    val maximum = Math.max(incoming * 50, outgoingWidth / (outgoingEdgesOf.size() + 1))
  //    Math.min(1000, maximum)
  //  }

  def getCellWidthBasedOnLabel(vertexLabel: String): Int = {
    vertexLabel.length * 5 + 20
  }

  override def isCellMovable(cell: scala.Any): Boolean = {
    cell match {
      case myCell: mxCell if myCell.isEdge => false
      case _ => super.isCellEditable(cell)
    }
  }

  override def isCellEditable(cell: scala.Any): Boolean = {
    cell match {
      case myCell: mxCell if myCell.isEdge => false
      case _ => super.isCellEditable(cell)
    }
  }
}
