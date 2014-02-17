package application

import scala.swing._
import graphing.TransformationGraph
import org.jgrapht.ext.JGraphModelAdapter
import com.mxgraph.view.mxGraph
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import scala.collection.convert.Wrappers._
import scala.collection.mutable
import com.mxgraph.model.mxCell

class myGraph extends mxGraph {

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

object Program extends SimpleSwingApplication {

  def mxGraphFromGraphT[V,E](origin: org.jgrapht.Graph[V,E]) =
  {
    val graph = new myGraph()
    graph.setAutoSizeCells(true)
    graph.setCellsResizable(false)
    graph.setConnectableEdges(false)
    graph.setResetEdgesOnMove(true)

    val vertexMap = mutable.Map[V,AnyRef]()
    val parent = graph.getDefaultParent
    for(vertex <- new JSetWrapper(origin.vertexSet))
    {
      val cell = graph.insertVertex(parent, null, "", 20, 20, 80, 30).asInstanceOf[mxCell]
      cell.setValue(vertex.toString)
      vertexMap.put(vertex, cell)
    }
    for(edge <- new JSetWrapper(origin.edgeSet()))
    {
      graph.insertEdge(parent, null, "", vertexMap(origin.getEdgeSource(edge)), vertexMap(origin.getEdgeTarget(edge))).asInstanceOf[mxCell]
    }
    graph
  }

  def top = new MainFrame {
    maximize()
    title = "Modular Compiler"

    val graph = TransformationGraph.getGraph

    val graphModelAdapter = new JGraphModelAdapter( graph )

    //val jGraph = new JGraph(graphModelAdapter)
    val mxGraph = mxGraphFromGraphT(graph)
    val layout = new mxHierarchicalLayout(mxGraph)
    layout.execute(mxGraph.getDefaultParent)
    val graphComponent = new mxGraphComponent(mxGraph)
    graphComponent.setConnectable(false)
    graphComponent.setEnabled(true)
    contents = Component.wrap(graphComponent)
    //contents = Component.wrap(jGraph)
  }
}
