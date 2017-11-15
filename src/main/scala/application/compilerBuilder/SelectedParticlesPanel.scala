package application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.{MouseEvent, ActionEvent, ActionListener}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import core.particles.Delta
import org.jdesktop.swingx.JXList

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

class DeltaInstanceJXList() extends JXList() {
  override def getToolTipText(event: MouseEvent): String = {
    val index = this.locationToIndex(event.getPoint)
    val model = this.getModel
    if (index >= 0)
      model.getElementAt(index).asInstanceOf[DeltaInstance].delta.description
    else
      ""
  }
}

object SelectedParticlesPanel {
  def getPanel(panel: CompilerBuilderPanel, compilerParticles: DefaultListModel[DeltaInstance]) = {
    val compilerList = new DeltaInstanceJXList()
    compilerList.setTransferHandler(new SelectedParticlesTransferHandler(compilerList, compilerParticles))
    compilerList.setDropMode(DropMode.INSERT)
    compilerList.setModel(compilerParticles)
    compilerList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    val compilerListPanel = panel.getInjectorListVisuals(compilerList)
    compilerListPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Selected"))

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
