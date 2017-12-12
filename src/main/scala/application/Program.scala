package application

import javax.swing._

import application.compilerBuilder.CompilerBuilderPanel
import application.graphing.GraphView
import application.graphing.model.DeltaGraph
import com.mxgraph.swing.mxGraphComponent

import scala.swing.{Component, MainFrame, SimpleSwingApplication}

object Program extends SimpleSwingApplication {


  def top = new MainFrame {
    maximize()
    title = "Blender"

    val tabbedPane = new JTabbedPane()
    
    val compilerBuilder = new CompilerBuilderPanel()
    tabbedPane.add("Language construction workbench", compilerBuilder)

    val architecturePanel = getGraphComponent
    tabbedPane.add("Delta dependency graph", architecturePanel)

    contents = Component.wrap(tabbedPane)
    //architecturePanel.scrollToCenter(true)
  }

  def getGraphComponent = {
    val graph = new DeltaGraph()

    val mxGraph = new GraphView(graph)

    val graphComponent = new mxGraphComponent(mxGraph)
    graphComponent.setConnectable(false)
    graphComponent.setEnabled(true)
    graphComponent
  }
}
