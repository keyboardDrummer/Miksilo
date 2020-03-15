package miksilo.playground.application

import java.awt.event.{FocusEvent, FocusListener}
import java.awt.{AWTEvent, EventQueue, Toolkit}
import javax.swing._

import miksilo.playground.application.compilerBuilder.LanguageWorkbench
import miksilo.playground.application.graphing.GraphView
import miksilo.playground.application.graphing.model.DeltaGraph
import com.mxgraph.swing.mxGraphComponent
import org.oxbow.swingbits.dialog.task.TaskDialogs

import scala.swing.{Component, MainFrame, SimpleSwingApplication}

object Program extends SimpleSwingApplication {

  Toolkit.getDefaultToolkit.getSystemEventQueue.push(new ShowExceptionsInDialog)

  def top: MainFrame = new MainFrame {
    maximize()
    title = "Miksilo"

    val tabbedPane = new JTabbedPane()
    
    val compilerBuilder = new LanguageWorkbench()
    tabbedPane.add("Language builder", compilerBuilder)
    tabbedPane.setFont(StyleSheet.tabFont)

    val dependencyGraph = getDependencyGraph
    tabbedPane.add("Delta dependency graph", dependencyGraph)

    contents = Component.wrap(tabbedPane)
  }


  def getDependencyGraph: mxGraphComponent = {
    val graph = new DeltaGraph()

    val mxGraph = new GraphView(graph)

    val graphComponent = new mxGraphComponent(mxGraph)
    graphComponent.setConnectable(false)
    graphComponent.setEnabled(true)
    graphComponent.addFocusListener(new FocusListener {
      var firstTime = true

      override def focusGained(e: FocusEvent): Unit = {
        if (firstTime) {
          graphComponent.scrollToCenter(true)
          graphComponent.scrollToCenter(false)
          firstTime = false
        }
      }

      override def focusLost(e: FocusEvent): Unit = {}
    })
    graphComponent
  }
}
