package application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import javax.swing.event.{ListDataEvent, ListDataListener, ListSelectionEvent, ListSelectionListener}

import application.graphing.model.DepthFirstTraversal
import core.transformation.sillyCodePieces.Particle
import org.jdesktop.swingx.JXList

import scala.collection.convert.Wrappers.JEnumerationWrapper

object MissingParticlesPanel {

  def getPanel(panel: CompilerBuilderPanel, compilerParticles: DefaultListModel[Particle]) = {
    val dependentItems = new DefaultListModel[Particle]()
    val dependentList = new ParticleList()
    dependentList.setModel(dependentItems)
    dependentList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    dependentList.setTransferHandler(new ParticleProviderTransferHandler(dependentList))
    dependentList.setDragEnabled(true)
    val dependentPanel = panel.getInjectorListVisuals(dependentList)
    dependentPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Missing"))


    val addButton = new JButton("Add")
    dependentList.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e: ListSelectionEvent): Unit = addButton.setEnabled(dependentList.getSelectedValues.nonEmpty)
    })
    addButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        for (selectedValue <- dependentList.getSelectedValues)
          compilerParticles.addElement(selectedValue.asInstanceOf[Particle])
      }
    })
    dependentPanel.add(addButton, BorderLayout.PAGE_END)

    def setDependentItems() = {
      dependentItems.clear()
      val missingDependencies = getMissingDependencies(JEnumerationWrapper(compilerParticles.elements()).toSeq)
      for (missingDependency <- missingDependencies)
        dependentItems.addElement(missingDependency)
    }

    compilerParticles.addListDataListener(new ListDataListener {

      override def intervalRemoved(e: ListDataEvent): Unit = setDependentItems()

      override def intervalAdded(e: ListDataEvent): Unit = setDependentItems()

      override def contentsChanged(e: ListDataEvent): Unit = setDependentItems()
    })
    dependentPanel
  }

  def getMissingDependencies(transformations: Seq[Particle]): Seq[Particle] = {
    var result = List.empty[Particle]
    val transformationSet = transformations.toSet
    DepthFirstTraversal.traverse[Particle](transformations,
      t => t.dependencies.collect({case x:Particle => x}),
      t => {}, t => result = t :: result)
    result.filter(e => !transformationSet.contains(e))
  }
}
