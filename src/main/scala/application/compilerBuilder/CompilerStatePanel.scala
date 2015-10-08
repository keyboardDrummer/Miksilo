package application.compilerBuilder

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import application.StyleSheet
import application.compilerCockpit.CompilerCockpit
import core.particles.Particle

import scala.collection.convert.Wrappers.JEnumerationWrapper

class CompilerStatePanel(panel: CompilerBuilderPanel) extends JPanel(new GridBagLayout()) {
  val compilerParticles = new DefaultListModel[Particle]()

  StyleSheet.setTitleBorder(this, "Compiler")

  val firstPanel: JPanel = getCompilerTopPanel

  val constraints = getConstraints
  constraints.gridx = 0
  constraints.weighty = 2
  add(firstPanel, constraints)

  val consolePanel = new ConsolePanel(compilerParticles)

  constraints.weighty = 1
  add(consolePanel, constraints)

  val actionButtons: JPanel = getActionButtonPanel

  constraints.weighty = 0
  add(actionButtons, constraints)

  def getActionButtonPanel: JPanel = {
    val actionButtonsLayout = new FlowLayout()
    actionButtonsLayout.setAlignment(FlowLayout.RIGHT)
    val actionButtons = new JPanel(actionButtonsLayout)
    val launchCockpitButton = new JButton("Launch Cockpit")
    launchCockpitButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val particlesAsSequence = new JEnumerationWrapper(compilerParticles.elements()).toList
        val cockpit = new CompilerCockpit(particlesAsSequence)
        cockpit.pack()
        cockpit.maximize()
        cockpit.visible = true
      }
    })
    actionButtons.add(launchCockpitButton)
    actionButtons
  }

  def getCompilerTopPanel: JPanel = {
    val firstPanel = new JPanel(new GridBagLayout())

    val compilerListPanel = SelectedParticlesPanel.getPanel(panel, compilerParticles)
    val compilerListConstraints = getConstraints
    compilerListConstraints.gridx = 0
    firstPanel.add(compilerListPanel, compilerListConstraints)

    val dependentPanel: JPanel = MissingParticlesPanel.getPanel(panel, compilerParticles)
    val dependentConstraints = getConstraints
    dependentConstraints.gridx = 1
    firstPanel.add(dependentPanel, dependentConstraints)
    firstPanel
  }

  def getConstraints: GridBagConstraints = {
    val cons = new GridBagConstraints()
    cons.fill = GridBagConstraints.BOTH
    cons.weightx = 1
    cons.weighty = 1
    cons
  }
}
