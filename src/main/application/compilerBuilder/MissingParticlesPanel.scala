package application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import javax.swing.event.{ListDataEvent, ListDataListener, ListSelectionEvent, ListSelectionListener}

import core.transformation.sillyCodePieces.Injector
import org.jdesktop.swingx.JXList

import scala.collection.convert.Wrappers.JEnumerationWrapper
import scala.collection.mutable

object MissingParticlesPanel {

  def getPanel(compilerParticles: DefaultListModel[Injector]) = {
    val dependentItems = new DefaultListModel[Injector]()
    val dependentList = new JXList()
    dependentList.setModel(dependentItems)
    dependentList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    val dependentPanel = CompilerBuilderPanel.getInjectorListVisuals(dependentList)
    dependentPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Missing"))

    val addButton = new JButton("Add")
    dependentList.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e: ListSelectionEvent): Unit = addButton.setEnabled(dependentList.getSelectedValues.nonEmpty)
    })
    addButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        for (selectedValue <- dependentList.getSelectedValues)
          compilerParticles.addElement(selectedValue.asInstanceOf[Injector])
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

  def getMissingDependencies(transformations: Seq[Injector]): Seq[Injector] = {
    var available = transformations.toSet[Injector]
    var result = Seq.empty[Injector]
    val queue: mutable.Queue[Injector] = mutable.Queue(transformations.reverse: _*)
    while (queue.nonEmpty) {
      val transformation = queue.dequeue()
      val newMissingDependencies: Seq[Injector] = transformation.dependencies.collect({ case contract: Injector => contract}).diff(available).toSeq
      queue.enqueue(newMissingDependencies: _*)
      result ++= newMissingDependencies
      available += transformation
      available ++= newMissingDependencies
    }
    result.distinct
  }
}
