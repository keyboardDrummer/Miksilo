package application.compilerCockpit

import java.awt._
import javax.swing._
import javax.swing.text.PlainDocument

import application.StyleSheet
import core.layouts.SwingEquationLayout
import core.transformation.sillyCodePieces.Injector

import scala.swing.{Component, Frame}

class CompilerCockpit(val transformations: Seq[Injector]) extends Frame {

  private val inputDocument = new PlainDocument()
  private val outputDocument = new PlainDocument()
  val textAreaInput: TextAreaInput = new TextAreaInput(() => inputDocument.getText(0, inputDocument.getLength))

  val textAreaOutput: TextAreaOutput = new TextAreaOutput(s => setOutputText(s))
  val compileOptions = Array(CompileByteCode, CompileAndRun)
  val outputOptions = Array[OutputOption](textAreaOutput)
  val inputOptions = Array[InputOption](textAreaInput)

  var inputOptionModel = new DefaultComboBoxModel[InputOption](inputOptions)
  inputOptionModel.setSelectedItem(textAreaInput)

  val outputOptionModel = new DefaultComboBoxModel[OutputOption](outputOptions)
  outputOptionModel.setSelectedItem(textAreaOutput)

  val compileOptionModel = new DefaultComboBoxModel[CompileOption](compileOptions)
  compileOptionModel.setSelectedItem(CompileAndRun)

  initialise()

  def outputOption = outputOptionModel.getSelectedItem.asInstanceOf[OutputOption]
  def compileOption: CompileOption = compileOptionModel.getSelectedItem.asInstanceOf[CompileOption]
  def inputOption = inputOptionModel.getSelectedItem.asInstanceOf[InputOption]

  def setOutputText(text: String) {
    outputDocument.replace(0, outputDocument.getLength, text, null)
  }

  def initialise() {
    val panel = new JPanel()
    val layout = new GroupLayout(panel)
    panel.setLayout(layout)
    contents = Component.wrap(panel)

    val equationLayout = new SwingEquationLayout(panel)

    val chooseInputSwing: JPanel = getChooseInput
    val chooseInput = equationLayout.addComponent(chooseInputSwing)
    val chooseCompileSwing: JPanel = getChooseCompile
    val chooseCompile = equationLayout.addComponent(chooseCompileSwing)
    val chooseOutputSwing: JPanel = getChooseOutput
    val chooseOutput = equationLayout.addComponent(chooseOutputSwing)
    val executeButtonSwing: JButton = new ExecuteButton(this)
    val executeButton = equationLayout.addComponent(executeButtonSwing)
    val inputPanel = equationLayout.addComponent(getInputPanel)
    val outputPanel = equationLayout.addComponent(getOutputPanel)
    val grammarButtonSwing: JButton = new ShowInputGrammarButton(this)
    val grammarButton = equationLayout.addComponent(grammarButtonSwing)

    val innerLayout = equationLayout.equationLayout

    equationLayout.makePreferredSize(chooseCompile)
    equationLayout.makePreferredSize(chooseOutput)
    equationLayout.makePreferredSize(chooseInput)
    equationLayout.makePreferredSize(grammarButton)

    //HORIZONTAL
    innerLayout.addLeftToRight(innerLayout.container, inputPanel, executeButton, outputPanel, innerLayout.container)
    innerLayout.expressions += inputPanel.width - outputPanel.width
    innerLayout.expressions += executeButton.width - executeButtonSwing.getPreferredSize.width

    innerLayout.expressions ++= Seq(chooseInput.horizontalCenter2 - inputPanel.horizontalCenter2,
      chooseCompile.horizontalCenter2 - executeButton.horizontalCenter2,
      chooseOutput.horizontalCenter2 - outputPanel.horizontalCenter2)

    innerLayout.addEquals(grammarButton.left, 0)

    //VERTICAL
    innerLayout.addEquals(chooseInput.verticalCenter2, chooseCompile.verticalCenter2, chooseOutput.verticalCenter2, grammarButton.verticalCenter2)
    innerLayout.addRow(inputPanel, executeButton, outputPanel)
    innerLayout.addTopToBottom(innerLayout.container, chooseInput, inputPanel, innerLayout.container)
  }

  def getInputPanel: JPanel = {
    val cardLayout = new CardLayout()
    val panel = new JPanel(cardLayout)
    val inputTextArea = new JTextArea(inputDocument)
    inputTextArea.setFont(StyleSheet.codeFont)
    inputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())
    panel.add(new JScrollPane(inputTextArea))
    panel
  }

  def getOutputPanel: JPanel = {
    val cardLayout = new CardLayout()
    val outputPanel = new JPanel(cardLayout)
    val outputTextArea = new JTextArea(outputDocument)
    outputTextArea.setFont(StyleSheet.codeFont)
    outputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())
    outputPanel.add(new JScrollPane(outputTextArea))
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
    val inputComboBox: JComboBox[InputOption] = new JComboBox(inputOptionModel)
    chooseInput.add(inputComboBox)
    chooseInput
  }

  def getChooseCompile: JPanel = {
    val chooseCompile = new JPanel()
    chooseCompile.add(new JLabel("Compile:"))
    val compileComboBox: JComboBox[CompileOption] = new JComboBox(compileOptionModel)
    chooseCompile.add(compileComboBox)
    chooseCompile
  }

  def getChooseOutput: JPanel = {
    val chooseOutput = new JPanel()
    chooseOutput.add(new JLabel("Output:"))

    val chooseOutputComboBox = new JComboBox(outputOptionModel)
    chooseOutput.add(chooseOutputComboBox)
    chooseOutput
  }
}
