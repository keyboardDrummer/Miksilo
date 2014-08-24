package application

import javax.swing._

import application.compilerBuilder.CompilerBuilderPanel
import application.graphing.GraphView
import application.graphing.model.TransformationGraph
import com.mxgraph.swing.mxGraphComponent

import scala.swing.{Component, MainFrame, SimpleSwingApplication}

object Program extends SimpleSwingApplication {


  def top = new MainFrame {
    maximize()
    title = "Modular Compiler"

    val graphComponent = getGraphComponent
    val tabbedPane = new JTabbedPane()
    tabbedPane.add("Architecture", graphComponent)

    val panel = new CompilerBuilderPanel()
    tabbedPane.add("Compiler Builder", panel)

    contents = Component.wrap(tabbedPane)
    graphComponent.scrollToCenter(true)
  }

  def getGraphComponent = {
    val graph = new TransformationGraph()

    val mxGraph = new GraphView(graph)

    val graphComponent = new mxGraphComponent(mxGraph)
    graphComponent.setConnectable(false)
    graphComponent.setEnabled(true)
    graphComponent
  }
}
