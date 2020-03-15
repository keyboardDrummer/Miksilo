package playground.application.compilerBuilder

import java.awt._
import javax.swing._
import javax.swing.border.BevelBorder
import javax.swing.event.ListSelectionEvent

import playground.application.compilerBuilder.DeltaInstance._
import playground.application.compilerCockpit.MarkOutputGrammar
import playground.application.{InjectorListCellRenderer, StyleSheet}
import core.deltas.Delta
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import org.jdesktop.swingx.JXList

object LanguageWorkbench {
  val availableDeltas: Set[Delta] = JavaToByteCodeLanguage.allDeltas ++ Seq(MarkOutputGrammar)
}

class LanguageWorkbench extends JPanel(new GridBagLayout()) {

  val painter = new DeltaLabelPainter(this, LanguageWorkbench.availableDeltas)

  setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
  setAutoscrolls(true)

  val programPanel = new CurrentLanguagePanel(this)
  val compilerParticles = programPanel.selectedParticles
  val presetsPanel: JPanel = new PresetsPanel(programPanel.compilerName, compilerParticles)

  val presetsConstraints = getConstraints
  add(presetsPanel, presetsConstraints)

  val availableScrollPane = getAvailableScrollPane
  val availableListConstraints: GridBagConstraints = getConstraints
  add(availableScrollPane, availableListConstraints)

  val programPanelConstraints = getConstraints
  programPanelConstraints.weightx = if (!StyleSheet.presentationMode) 2 else 1
  add(programPanel, programPanelConstraints)

  def getInjectorListVisuals(list: JXList): JPanel = {

    val result = new JPanel(new BorderLayout())

    list.setAutoCreateRowSorter(true)

    list.addListSelectionListener((e: ListSelectionEvent) => {
      painter.select(list.getSelectedValues.map(i => i.getParticle).toSeq)
    })

    val listener = new SearchFieldListener(list)
    val searchField = new JTextField(20)
    searchField.getDocument.addDocumentListener(listener)
    result.add(searchField, BorderLayout.PAGE_START)

    val scrollPane: JScrollPane = StyleSheet.getAnyListVisuals(list)
    new InjectorListCellRenderer(painter).setInJXList(list)

    result.add(scrollPane, BorderLayout.CENTER)

    result
  }

  def getAvailableScrollPane: JPanel = {
    val availableItems: Seq[Delta] = LanguageWorkbench.availableDeltas.toSeq.sortBy(i => i.name)
    val availableList = new ParticleList()
    availableList.setModel(new AbstractListModel[Delta] {
      def getSize: Int = availableItems.length
      def getElementAt(i: Int): Delta = availableItems(i)
    })

    availableList.setTransferHandler(new DeltaProviderTransferHandler(availableList))
    availableList.setDragEnabled(true)

    val result = getInjectorListVisuals(availableList)
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
