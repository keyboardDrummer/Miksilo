package application.compilerBuilder

import java.awt._
import javax.swing._
import javax.swing.border.BevelBorder

import application.{ExampleListCellRenderer, StyleSheet}
import core.transformation.sillyCodePieces.Injector
import org.jdesktop.swingx.JXList
import transformations.javac.JavaCompiler

object CompilerBuilderPanel {

  def getInjectorListVisuals(list: JXList) = {

    val result = new JPanel(new BorderLayout())

    list.setAutoCreateRowSorter(true)
    val listener = new SearchFieldListener(list)
    val searchField = new JTextField(20)
    searchField.getDocument.addDocumentListener(listener)
    result.add(searchField, BorderLayout.PAGE_START)

    val scrollPane: JScrollPane = StyleSheet.getAnyListVisuals(list)
    list.setCellRenderer(new ExampleListCellRenderer())
    scrollPane

    result.add(scrollPane, BorderLayout.CENTER)

    result
  }

}

class CompilerBuilderPanel extends JPanel(new GridBagLayout()) {

  setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
  setAutoscrolls(true)

  val programPanel = new CompilerStatePanel()
  val compilerParticles = programPanel.compilerParticles
  val presetsPanel: JPanel = new PresetsPanel(compilerParticles)

  val presetsConstraints = getConstraints
  add(presetsPanel, presetsConstraints)

  val availableScrollPane = getAvailableScrollPane
  val availableListConstraints: GridBagConstraints = getConstraints
  add(availableScrollPane, availableListConstraints)

  val programPanelConstraints = getConstraints
  programPanelConstraints.weightx = 2
  add(programPanel, programPanelConstraints)

  def getAvailableScrollPane = {
    val availableItems: Seq[Injector] = JavaCompiler.javaCompilerTransformations
    val availableList = new JXList(availableItems.toArray.asInstanceOf[Array[Object]])

    availableList.setTransferHandler(new ParticleProviderTransferHandler(availableList))
    availableList.setDragEnabled(true)

    val result = CompilerBuilderPanel.getInjectorListVisuals(availableList)
    StyleSheet.setTitleBorder(result, "Available")
    result
  }


  def getConstraints: GridBagConstraints = {
    val cons = new GridBagConstraints()
    cons.fill = GridBagConstraints.BOTH
    cons.weightx = 1
    cons.weighty = 1
    cons
  }
}
