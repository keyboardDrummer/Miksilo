package application.compilerBuilder

import java.awt.BorderLayout
import java.awt.event.{MouseEvent, ActionEvent, ActionListener}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import core.particles.Particle
import org.jdesktop.swingx.JXList

object ParticleInstance
{
  implicit class ParticleLike(val particleLike: Any)
  {
    def getParticle: Particle = particleLike match {
      case particle: Particle => particle
      case instance: ParticleInstance => instance.particle
    }

    def getParticleInstance: ParticleInstance = particleLike match {
      case particle: Particle => new ParticleInstance(particle)
      case instance: ParticleInstance => instance
    }
  }
}

class ParticleInstance(val particle: Particle)

class ParticleInstanceJXList() extends JXList() {
  override def getToolTipText(event: MouseEvent): String = {
    val index = this.locationToIndex(event.getPoint)
    val model = this.getModel
    if (index >= 0)
      model.getElementAt(index).asInstanceOf[ParticleInstance].particle.description
    else
      ""
  }
}

object SelectedParticlesPanel {
  def getPanel(panel: CompilerBuilderPanel, compilerParticles: DefaultListModel[ParticleInstance]) = {
    val compilerList = new ParticleInstanceJXList()
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
