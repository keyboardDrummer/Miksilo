package miksilo.playground.application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.ActionEvent

import javax.swing._
import javax.swing.event.ListSelectionEvent
import miksilo.modularLanguages.core.deltas.Delta
import miksilo.playground.application.StyleSheet

object DeltaInstance
{
  implicit class ParticleLike(val particleLike: Any)
  {
    def getParticle: Delta = particleLike match {
      case particle: Delta => particle
      case instance: DeltaInstance => instance.delta
    }

    def getParticleInstance: DeltaInstance = particleLike match {
      case particle: Delta => new DeltaInstance(particle)
      case instance: DeltaInstance => instance
    }
  }
}

class DeltaInstance(val delta: Delta)

object SelectedDeltasPanel {
  def getPanel(panel: LanguageWorkbench, compilerParticles: DefaultListModel[DeltaInstance]): JPanel = {
    val compilerList = new ParticleList()
    compilerList.setTransferHandler(new SelectedDeltasTransferHandler(compilerList, compilerParticles))
    compilerList.setDropMode(DropMode.INSERT)
    compilerList.setModel(compilerParticles)
    compilerList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    val compilerListPanel = panel.getInjectorListVisuals(compilerList)
    val titledBorder = BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Selected")
    titledBorder.setTitleFont(StyleSheet.defaultFont)
    compilerListPanel.setBorder(titledBorder)

    val removeButton = new JButton("Remove")
    removeButton.setFont(StyleSheet.defaultFont)
    compilerList.addListSelectionListener((e: ListSelectionEvent) =>
      removeButton.setEnabled(compilerList.getSelectedValues.nonEmpty))

    removeButton.addActionListener((e: ActionEvent) => {
      for (selectedValue <- compilerList.getSelectedValues)
        compilerParticles.removeElement(selectedValue)
    })
    compilerListPanel.add(removeButton, BorderLayout.PAGE_END)
    compilerListPanel
  }
}
