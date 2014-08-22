package application.compilerBuilder.compilerCockpit

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import javax.swing.text.PlainDocument

import application.StyleSheet
import core.modularProgram.PieceCombiner
import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector

import scala.swing.{Component, Frame}

class CompilerCockpit(transformations: Seq[Injector]) extends Frame {

  private val inputDocument = new PlainDocument()
  private val outputDocument = new PlainDocument()
  private var inputOption: InputOption = new TextAreaInput(() => inputDocument.getText(0,inputDocument.getLength))
  val textAreaOutput: TextAreaOutput = new TextAreaOutput(s => setOutputText(s))
  private var outputOption: OutputOption = {
    textAreaOutput
  }
  val minimumTextAreaWidth = 600

  private var compileOption: CompileOption = CompileAndRun
  val compileOptions = Array(CompileByteCode, compileOption)
  val inputOptions = Array[InputOption](inputOption)
  val outputOptions = Array[OutputOption](outputOption)

  initialise()

  def setOutputText(text: String) {
    outputDocument.replace(0, outputDocument.getLength, text, null)
  }

  def initialise() {
    val panel = new JPanel(new GridBagLayout())
    contents = Component.wrap(panel)

    val constraints = getConstraints

    constraints.gridx = 0
    panel.add(getChooseInput,constraints)

    constraints.gridx = 1
    panel.add(getChooseCompile, constraints)

    constraints.gridx = 2
    panel.add(getChooseOutput, constraints)

    constraints.fill = GridBagConstraints.BOTH
    constraints.gridy = 1
    constraints.gridx = 1
    panel.add(getExecuteButton, constraints)

    constraints.fill = GridBagConstraints.BOTH
    constraints.weightx = 1
    constraints.weighty = 1
    constraints.gridy = 1
    constraints.gridx = 0
    panel.add(getInputPanel, constraints)

    constraints.gridx = 2
    panel.add(getOutputPanel, constraints)

  }

  def getExecuteButton: JButton = {
    val executeButton = new JButton("EXECUTE")
    executeButton.setFont(StyleSheet.hugeFont)
    executeButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val pieces = Seq(inputOption) ++ transformations ++ Seq(compileOption, outputOption)
        val state = new TransformationState()
        PieceCombiner.combineAndExecute(state, pieces.reverse)
      }
    })
    executeButton
  }

  def getShowInputGrammarButton: JButton = {
    val executeButton = new JButton("Show grammar")
    executeButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val pieces = Seq(InputGrammarToOutput, textAreaOutput) ++ transformations
        val state = new TransformationState()
        PieceCombiner.combineAndExecute(state, pieces.reverse)
      }
    })
    executeButton
  }

  def getInputPanel: JPanel = {
    val cardLayout = new CardLayout()
    val panel = new JPanel(cardLayout)
    val inputTextArea = new JTextArea()
    inputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())
    panel.add(new JScrollPane(inputTextArea))
    panel.setMinimumSize(new Dimension(minimumTextAreaWidth, 0))
    panel
  }

  def getOutputPanel: JPanel = {
    val cardLayout = new CardLayout()
    val outputPanel = new JPanel(cardLayout)
    val outputTextArea = new JTextArea(outputDocument)
    outputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())
    outputPanel.add(new JScrollPane(outputTextArea))
    outputPanel.setMinimumSize(new Dimension(minimumTextAreaWidth, 0))
    outputPanel
  }

  def getConstraints = {
    val constraints = new GridBagConstraints()
    constraints.insets = new Insets(3, 3, 3, 3)
    constraints
  }

  def getChooseInput = {
    val chooseInput = new JPanel()
    chooseInput.add(new JLabel("Input:"))
    val inputComboBox: JComboBox[InputOption] = new JComboBox(new DefaultComboBoxModel[InputOption](inputOptions) {
      override def setSelectedItem(selectedItem: scala.Any): Unit = inputOption = selectedItem.asInstanceOf[InputOption]
    })
    chooseInput.add(inputComboBox)
    chooseInput.add(getShowInputGrammarButton)
    chooseInput
  }

  def getChooseCompile: JPanel = {
    val chooseCompile = new JPanel()
    chooseCompile.add(new JLabel("Compile:"))
    val compileComboBox: JComboBox[CompileOption] = new JComboBox(new DefaultComboBoxModel[CompileOption](compileOptions) {
      override def setSelectedItem(selectedItem: scala.Any): Unit = compileOption = selectedItem.asInstanceOf[CompileOption]
    })
    chooseCompile.add(compileComboBox)
    chooseCompile
  }

  def getChooseOutput: JPanel = {
    val chooseOutput = new JPanel()
    chooseOutput.add(new JLabel("Output:"))

    val chooseOutputComboBox = new JComboBox(new DefaultComboBoxModel[OutputOption](outputOptions) {
      override def setSelectedItem(selectedItem: scala.Any): Unit = outputOption = selectedItem.asInstanceOf[OutputOption]
    })
    chooseOutput.add(chooseOutputComboBox)
    chooseOutput
  }
}
