package application.compilerBuilder

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import javax.swing.text.PlainDocument

import application.StyleSheet
import application.compilerCockpit.CompilerCockpit
import core.particles.Particle

import scala.collection.convert.Wrappers.JEnumerationWrapper

class ParticleInstanceList extends DefaultListModel[ParticleInstance]
{
  def scalaElements: Seq[Particle] = JEnumerationWrapper(super.elements()).map(instance => instance.particle).toSeq
}

class CompilerStatePanel(panel: CompilerBuilderPanel) extends JPanel(new GridBagLayout()) {
  val selectedParticles = new ParticleInstanceList()
  val compilerName = new PlainDocument()

  StyleSheet.setTitleBorder(this, "Compiler")

  val firstPanel: JPanel = getCompilerTopPanel

  val constraints = getConstraints
  constraints.gridx = 0
  constraints.weighty = 2
  add(firstPanel, constraints)

  val consolePanel = new ConsolePanel(selectedParticles)

  constraints.weighty = 1
  add(consolePanel, constraints)

  val actionButtons: JPanel = getActionButtonPanel

  constraints.weighty = 0
  add(actionButtons, constraints)

  def getActionButtonPanel: JPanel = {
    val actionButtonsLayout = new FlowLayout()
    actionButtonsLayout.setAlignment(FlowLayout.RIGHT)
    val actionButtons = new JPanel(actionButtonsLayout)
    val launchCockpitButton = new JButton("Build")
    launchCockpitButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val cockpit = new CompilerCockpit(compilerName.getText(0,compilerName.getLength), selectedParticles.scalaElements)
        cockpit.pack()
        cockpit.maximize()
        cockpit.visible = true
      }
    })
    val compilerNameField = new JTextField(20)
    compilerNameField.setDocument(compilerName)
    actionButtons.add(new JLabel("Name:"))
    actionButtons.add(compilerNameField)
    actionButtons.add(launchCockpitButton)
    actionButtons
  }

  def getCompilerTopPanel: JPanel = {
    val firstPanel = new JPanel(new GridBagLayout())

    val compilerListPanel = SelectedParticlesPanel.getPanel(panel, selectedParticles)
    val compilerListConstraints = getConstraints
    compilerListConstraints.gridx = 0
    firstPanel.add(compilerListPanel, compilerListConstraints)

    if (!StyleSheet.presentationMode)
    {
      val dependentPanel: JPanel = MissingParticlesPanel.getPanel(panel, selectedParticles)
      val dependentConstraints = getConstraints
      dependentConstraints.gridx = 1
      firstPanel.add(dependentPanel, dependentConstraints)
    }
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
