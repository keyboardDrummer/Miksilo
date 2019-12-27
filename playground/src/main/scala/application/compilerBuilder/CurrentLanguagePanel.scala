package application.compilerBuilder

import java.awt._
import java.awt.event.ActionEvent

import javax.swing._
import javax.swing.text.PlainDocument
import application.StyleSheet
import application.compilerCockpit.LanguageSandbox
import core.deltas.Delta

import scala.jdk.CollectionConverters

class DeltaInstanceList extends DefaultListModel[DeltaInstance]
{
  def scalaElements: Seq[Delta] = CollectionConverters.EnumerationHasAsScala(super.elements()).asScala.map(instance => instance.delta).toSeq
}

class CurrentLanguagePanel(panel: LanguageWorkbench) extends JPanel(new GridBagLayout()) {
  val selectedParticles = new DeltaInstanceList()
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
    launchCockpitButton.setFont(StyleSheet.defaultFont)
    launchCockpitButton.addActionListener((e: ActionEvent) => launchCockpit())
    val compilerNameField = new JTextField(15)
    compilerNameField.setDocument(compilerName)
    compilerNameField.setFont(StyleSheet.defaultFont)
    val nameLabel = new JLabel("Name:")
    nameLabel.setFont(StyleSheet.defaultFont)
    actionButtons.add(nameLabel)
    actionButtons.add(compilerNameField)
    actionButtons.add(launchCockpitButton)
    actionButtons
  }

  private def launchCockpit(): Unit = {
    val name = compilerName.getText(0, compilerName.getLength)
    val cockpit = new LanguageSandbox(name, selectedParticles.scalaElements)
    cockpit.pack()
    cockpit.maximize()
    cockpit.visible = true
  }

  def getCompilerTopPanel: JPanel = {
    val firstPanel = new JPanel(new GridBagLayout())

    val compilerListPanel = SelectedDeltasPanel.getPanel(panel, selectedParticles)
    val compilerListConstraints = getConstraints
    compilerListConstraints.gridx = 0
    firstPanel.add(compilerListPanel, compilerListConstraints)

    if (!StyleSheet.presentationMode)
    {
      val dependentPanel: JPanel = MissingDeltasPanel.getPanel(panel, selectedParticles)
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
