package application.compilerBuilder

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{GridBagConstraints, FlowLayout, BorderLayout, GridBagLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import application.StyleSheet
import application.compilerCockpit.CompilerCockpit
import core.transformation.sillyCodePieces.Injector
import org.jdesktop.swingx.JXList

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

    val compilerListPanel: JPanel = getCompilerListPanel
    val compilerListConstraints = getConstraints
    compilerListConstraints.gridx = 0
    firstPanel.add(compilerListPanel, compilerListConstraints)

    val dependentPanel: JPanel = getDependentPanel
    val dependentConstraints = getConstraints
    dependentConstraints.gridx = 1
    firstPanel.add(dependentPanel, dependentConstraints)
    firstPanel
  }

  def getDependentPanel: JPanel = {
    val dependentItems = new DefaultListModel[Injector]()
    val dependentList = new JXList()
    dependentList.setModel(dependentItems)
    val dependentPanel = CompilerBuilderPanel.getInjectorListVisuals(dependentList)
    dependentPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Automatically added"))
    dependentPanel
  }

  def getCompilerListPanel: JPanel = {
    val compilerList = new JXList()
    compilerList.setTransferHandler(new ChosenParticlesTransferHandler(compilerParticles))
    compilerList.setDropMode(DropMode.INSERT)
    compilerList.setModel(compilerParticles)
    compilerList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    val compilerListPanel = CompilerBuilderPanel.getInjectorListVisuals(compilerList)
    compilerListPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Chosen"))

    val removeButton = new JButton("Remove")
    compilerList.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e: ListSelectionEvent): Unit = removeButton.setEnabled(compilerList.getSelectedValues.nonEmpty)
    })
    removeButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        for(selectedValue <- compilerList.getSelectedValues)
          compilerParticles.removeElement(selectedValue)
      }
    })
    compilerListPanel.add(removeButton, BorderLayout.PAGE_END)
    compilerListPanel
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
