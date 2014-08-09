package application

import application.graphing.GraphView
import application.graphing.model.TransformationGraph
import com.mxgraph.swing.mxGraphComponent

import scala.swing._

object Program extends SimpleSwingApplication {


  def top = new MainFrame {
    maximize()
    title = "Modular Compiler"

    val graph = new TransformationGraph()

    val mxGraph = new GraphView(graph)

    val graphComponent = new mxGraphComponent(mxGraph)
    graphComponent.setConnectable(false)
    graphComponent.setEnabled(true)
    contents = Component.wrap(graphComponent)
  }
}
