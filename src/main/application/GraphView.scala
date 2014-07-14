package application

import java.util

import application.graphing.{TransformationGraph, TransformationVertex}
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.mxCell
import com.mxgraph.util.mxConstants
import com.mxgraph.view.{mxGraph, mxStylesheet}
import core.transformation.{Contract, GrammarTransformation, ProgramTransformation}

import scala.collection.convert.Wrappers.JSetWrapper
import scala.collection.mutable

class GraphView(origin: TransformationGraph) extends mxGraph {

  initialise()

  def initialise() {

    this.getModel.beginUpdate()

    setCellsResizable(false)
    setConnectableEdges(false)
    setResetEdgesOnMove(true)

    setStyleSheet()

    val vertexMap = mutable.Map[TransformationVertex, AnyRef]()
    val parent = getDefaultParent
    for (vertex <- new JSetWrapper(origin.vertexSet)) {
      val cell: mxCell = getVertex(parent, vertex)
      vertexMap.put(vertex, cell)
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
    val contractStyle = new util.Hashtable[String, Object]()
    contractStyle.put(mxConstants.STYLE_FILLCOLOR, "#FFFFFF")

    val grammarStyle = new util.Hashtable[String, Object]()
    grammarStyle.put(mxConstants.STYLE_FILLCOLOR, "#44E35C")

    stylesheet.putCellStyle("CONTRACT", contractStyle)
    stylesheet.putCellStyle("GRAMMAR", grammarStyle)
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

  def getCellWidthBasedOnDependencies(vertex: TransformationVertex) = {
    val incoming = vertex.transformation.dependencies.size
    val outgoing = origin.outgoingEdgesOf(vertex).size
    val maximum = Math.max(incoming, outgoing)
    maximum * 50
  }

  def getVertex(parent: AnyRef, vertex: TransformationVertex): mxCell = {
    val vertexLabel = vertex.toString
    val height: Double = 30
    val width = Math.max(getCellWidthBasedOnLabel(vertexLabel), getCellWidthBasedOnDependencies(vertex))
    val cell = insertVertex(parent, null, "", 20, 20, width, height).asInstanceOf[mxCell]
    cell.setValue(vertexLabel)
    vertex.transformation match {
      case _: GrammarTransformation =>
        cell.setStyle("GRAMMAR")

      case _: ProgramTransformation =>

      case _: Contract =>
        cell.setStyle("CONTRACT")
      case _ =>
    }

    cell
  }

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
