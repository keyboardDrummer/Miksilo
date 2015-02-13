package application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import core.transformation.sillyCodePieces.Particle
import org.jdesktop.swingx.JXList

object ChosenParticlesPanel {
  def getPanel(panel: CompilerBuilderPanel, compilerParticles: DefaultListModel[Particle]) = {
    val compilerList = new JXList()
    compilerList.setTransferHandler(new ChosenParticlesTransferHandler(compilerList, compilerParticles))
    compilerList.setDropMode(DropMode.INSERT)
    compilerList.setModel(compilerParticles)
    compilerList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    val compilerListPanel = panel.getInjectorListVisuals(compilerList)
    compilerListPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Chosen"))

    val removeButton = new JButton("Remove")
    compilerList.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e: ListSelectionEvent): Unit = removeButton.setEnabled(compilerList.getSelectedValues.nonEmpty)
    })
    removeButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        for (selectedValue <- compilerList.getSelectedValues)
          compilerParticles.removeElement(selectedValue)
      }
    })
    compilerListPanel.add(removeButton, BorderLayout.PAGE_END)
    compilerListPanel
  }
}
