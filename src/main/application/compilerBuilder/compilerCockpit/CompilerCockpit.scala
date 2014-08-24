package application.compilerBuilder.compilerCockpit

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import javax.swing.text.PlainDocument

import application.StyleSheet
import core.layouts.SwingEquationLayout
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

  private var compileOption: CompileOption = CompileAndRun
  val compileOptions = Array(CompileByteCode, compileOption)
  val inputOptions = Array[InputOption](inputOption)
  val outputOptions = Array[OutputOption](outputOption)

  initialise()

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
    val executeButtonSwing: JButton = getExecuteButton
    val executeButton = equationLayout.addComponent(executeButtonSwing)
    val inputPanel = equationLayout.addComponent(getInputPanel)
    val outputPanel = equationLayout.addComponent(getOutputPanel)
    val grammarButtonSwing: JButton = getShowInputGrammarButton
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
    val inputComboBox: JComboBox[InputOption] = new JComboBox(new DefaultComboBoxModel[InputOption](inputOptions) {
      override def setSelectedItem(selectedItem: scala.Any): Unit = inputOption = selectedItem.asInstanceOf[InputOption]
    })
    chooseInput.add(inputComboBox)
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
