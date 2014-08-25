package application.compilerBuilder

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, FlowLayout, GridBagConstraints, GridBagLayout}
import javax.swing._

import application.StyleSheet
import application.compilerCockpit.CompilerCockpit
import core.transformation.sillyCodePieces.Injector

import scala.collection.convert.Wrappers.JEnumerationWrapper

class CompilerStatePanel extends JPanel(new GridBagLayout()) {
  val compilerParticles = new DefaultListModel[Injector]()

  StyleSheet.setTitleBorder(this, "Compiler")

  val firstPanel: JPanel = getCompilerTopPanel

  val constraints = getConstraints
  constraints.gridx = 0
  constraints.weighty = 2
  add(firstPanel, constraints)

  add(new JSeparator(SwingConstants.HORIZONTAL), getSeparatorConstraints)
  val console = new JTextArea()
  console.setBorder(BorderFactory.createLoweredBevelBorder())
  val consoleWrapper = new JPanel(new BorderLayout())
  consoleWrapper.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Console"))
  consoleWrapper.add(console)

  constraints.weighty = 1
  add(consoleWrapper, constraints)

  val actionButtons: JPanel = getActionButtonPanel

  constraints.weighty = 0
  add(actionButtons, constraints)

  def getActionButtonPanel: JPanel = {
    val actionButtonsLayout = new FlowLayout()
    actionButtonsLayout.setAlignment(FlowLayout.RIGHT)
    val actionButtons = new JPanel(actionButtonsLayout)
    val buildCompilerButton = new JButton("Build Compiler")
    buildCompilerButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val cockpit = new CompilerCockpit(new JEnumerationWrapper(compilerParticles.elements()).toSeq)
        cockpit.pack()
        cockpit.maximize()
        cockpit.visible = true
      }
    })
    actionButtons.add(buildCompilerButton)
    actionButtons
  }

  def getCompilerTopPanel: JPanel = {
    val firstPanel = new JPanel(new GridBagLayout())

    val compilerListPanel: JPanel = ChosenParticlesPanel.getPanel(compilerParticles)
    val compilerListConstraints = getConstraints
    compilerListConstraints.gridx = 0
    firstPanel.add(compilerListPanel, compilerListConstraints)

    val dependentPanel: JPanel = MissingParticlesPanel.getPanel(compilerParticles)
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

  def getSeparatorConstraints: GridBagConstraints = {
    val separatorConstraints = new GridBagConstraints()
    separatorConstraints.fill = GridBagConstraints.BOTH
    separatorConstraints.insets = StyleSheet.defaultInsets
    separatorConstraints
  }
}
