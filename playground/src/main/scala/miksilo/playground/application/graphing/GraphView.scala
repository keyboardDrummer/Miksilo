package miksilo.playground.application.graphing

import java.util

import miksilo.playground.application.graphing.model.simplifications.DeltaGroup
import miksilo.playground.application.graphing.model.{DeltaGraph, DeltaVertex}
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.mxCell
import com.mxgraph.util.mxConstants
import com.mxgraph.view.{mxGraph, mxStylesheet}
import miksilo.modularLanguages.core.deltas.{DeltaWithGrammar, DeltaWithPhase}
import org.jgrapht.traverse.TopologicalOrderIterator

import scala.jdk.CollectionConverters

class GraphView(origin: DeltaGraph) extends mxGraph {

  initialise()

  def initialise(): Unit = {

    this.getModel.beginUpdate()

    setCellsResizable(false)
    setConnectableEdges(false)
    setResetEdgesOnMove(true)

    setStyleSheet()
    var vertexMap: Map[DeltaVertex, mxCell] = Map.empty[DeltaVertex, mxCell]
    val parent = getDefaultParent


    val topologicalOrdering = List.from(CollectionConverters.IteratorHasAsScala(new TopologicalOrderIterator(origin)).asScala).reverse
    for (vertex <- topologicalOrdering) {
      val cell: mxCell = getVertex(parent, vertex, vertexMap)
      vertexMap += (vertex -> cell)
    }
    for (edge <- CollectionConverters.SetHasAsScala(origin.edgeSet()).asScala) {
      val source = vertexMap(origin.getEdgeSource(edge))
      val target = vertexMap(origin.getEdgeTarget(edge))
      insertEdge(parent, source, target)
    }

    setLayout()

    this.getModel.endUpdate()
  }


  def setStyleSheet(): Unit =  {
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

  def setLayout(): Unit =  {
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

  def getVertex(parent: AnyRef, vertex: DeltaVertex, vertexMap: Map[DeltaVertex, mxCell]): mxCell = {
    val vertexLabel = vertex.contract.name
    val height: Double = 30
    val width = Math.max(getCellWidthBasedOnLabel(vertexLabel), getCellWidthBasedOnDependencies(vertex, vertexMap))
    val cell = insertVertex(parent, null, vertexLabel, 20, 20, width, height).asInstanceOf[mxCell]
    vertex.contract match {
      case _: DeltaGroup =>
        cell.setStyle("SIMPLIFICATION")

      case _: DeltaWithPhase =>
        cell.setStyle("TRANSFORMATION")

      case _: DeltaWithGrammar =>
        cell.setStyle("GRAMMAR")

      case _ =>
    }

    cell
  }


  def getCellWidthBasedOnDependencies(vertex: DeltaVertex, vertexMap: Map[DeltaVertex, mxCell]): Int = {
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
