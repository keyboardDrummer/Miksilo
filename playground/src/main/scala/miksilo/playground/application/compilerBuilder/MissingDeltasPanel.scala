package playground.application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.ActionEvent
import javax.swing._
import javax.swing.event.{ListDataEvent, ListDataListener, ListSelectionEvent}

import playground.application.StyleSheet
import playground.application.graphing.model.DepthFirstTraversal
import core.deltas.Delta

object MissingDeltasPanel {

  def getPanel(panel: LanguageWorkbench, selectedParticles: DeltaInstanceList): JPanel = {
    val dependentItems = new DefaultListModel[Delta]()
    val dependentList = new ParticleList()
    dependentList.setModel(dependentItems)
    dependentList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    dependentList.setTransferHandler(new DeltaProviderTransferHandler(dependentList))
    dependentList.setDragEnabled(true)
    val dependentPanel = panel.getInjectorListVisuals(dependentList)
    dependentPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Missing"))


    val addButton = new JButton("Add")
    addButton.setFont(StyleSheet.defaultFont)
    dependentList.addListSelectionListener((e: ListSelectionEvent) =>
      addButton.setEnabled(dependentList.getSelectedValues.nonEmpty))
    addButton.addActionListener((e: ActionEvent) => {
      for (selectedValue <- dependentList.getSelectedValues)
        selectedParticles.addElement(new DeltaInstance(selectedValue.asInstanceOf[Delta]))
    })
    dependentPanel.add(addButton, BorderLayout.PAGE_END)

    def setDependentItems(): Unit = {
      dependentItems.clear()
      val missingDependencies = getMissingDependencies(selectedParticles.scalaElements)
      for (missingDependency <- missingDependencies)
        dependentItems.addElement(missingDependency)
    }

    selectedParticles.addListDataListener(new ListDataListener {

      override def intervalRemoved(e: ListDataEvent): Unit = setDependentItems()

      override def intervalAdded(e: ListDataEvent): Unit = setDependentItems()

      override def contentsChanged(e: ListDataEvent): Unit = setDependentItems()
    })
    dependentPanel
  }

  def getMissingDependencies(transformations: Seq[Delta]): Seq[Delta] = {
    var result = List.empty[Delta]
    val transformationSet = transformations.toSet
    DepthFirstTraversal.traverse[Delta](transformations,
      t => t.dependencies.collect({case x:Delta => x}),
      t => {}, t => result = t :: result)
    result.filter(e => !transformationSet.contains(e))
  }
}
