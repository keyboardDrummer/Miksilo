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
    title = "Particle Compiler"

    val tabbedPane = new JTabbedPane()
    
    val compilerBuilder = new CompilerBuilderPanel()
    tabbedPane.add("Compiler Builder", compilerBuilder)

//    val architecturePanel = getGraphComponent
//    tabbedPane.add("Architecture", architecturePanel)

    contents = Component.wrap(tabbedPane)
    //architecturePanel.scrollToCenter(true)
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
