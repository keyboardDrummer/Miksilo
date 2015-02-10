package application.compilerCockpit

import java.awt._
import java.io.CharArrayWriter
import javax.swing._
import javax.swing.text.{DefaultCaret, PlainDocument}

import application.StyleSheet
import core.exceptions.CompileException
import core.layouts.SwingEquationLayout
import core.modularProgram.PieceCombiner
import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector
import transformations.bytecode.ByteCodeSkeleton

import scala.swing.{Component, Frame}
import scala.tools.nsc.NewLinePrintWriter
import scala.util.Try

class CompilerCockpit(val transformations: Seq[Injector]) extends Frame {

  private val inputDocument = new PlainDocument()
  private val outputDocument = new PlainDocument()
  val textAreaInput: ParseFromFunction = new ParseFromFunction(() => inputDocument.getText(0, inputDocument.getLength))

  val textAreaOutput: TextAreaOutput = new TextAreaOutput(s => setOutputText(s))
  val compileOptions = getCompileOptions.toArray

  def getCompileOptions: Seq[CompileOption] = {
    val selection = Set(CockpitOutputMarker, ByteCodeSkeleton)
    val orderedSelection = transformations.filter(o => selection.contains(o))
    val byteCodeActions = Seq(CompileAndRun, EmitByteCode) //if (orderedSelection.take(1) == Seq(ByteCodeSkeleton)) Seq(CompileAndRun, EmitByteCode) else Seq.empty
    byteCodeActions ++ Seq(PrettyPrint)
  }

  val outputOptions = Array[OutputOption](textAreaOutput)
  val inputOptions = Array[InputOption](textAreaInput)

  var inputOptionModel = new DefaultComboBoxModel[InputOption](inputOptions)
  inputOptionModel.setSelectedItem(textAreaInput)

  val outputOptionModel = new DefaultComboBoxModel[OutputOption](outputOptions)
  outputOptionModel.setSelectedItem(textAreaOutput)

  val compileOptionModel = new DefaultComboBoxModel[CompileOption](compileOptions)
  compileOptionModel.setSelectedItem(compileOptions(0))

  initialise()

  def outputOption = outputOptionModel.getSelectedItem.asInstanceOf[OutputOption]

  def compileOption: CompileOption = compileOptionModel.getSelectedItem.asInstanceOf[CompileOption]

  def inputOption = inputOptionModel.getSelectedItem.asInstanceOf[InputOption]

  def setOutputText(text: String) {
    outputDocument.replace(0, outputDocument.getLength, text, null)
  }

  def setInputText(text: String) {
    inputDocument.replace(0, inputDocument.getLength, text, null)
  }

  def execute(inputParticles: Seq[Injector], outputParticles: Seq[Injector]) = {
    val cockpitOutputActions = if (transformations.contains(CockpitOutputMarker)) Seq.empty else Seq(CockpitOutputMarker)
    val pieces = inputParticles ++ transformations ++ cockpitOutputActions
    val state = new TransformationState()
    CockpitOutputMarker.setState(state, outputParticles)
    Try(PieceCombiner.combineAndExecute(state, pieces.reverse)).
      recover({ case e: CompileException => setOutputText(e.toString) }).
      recover({ case e: Throwable =>
        val writer = new CharArrayWriter()
        e.printStackTrace(new NewLinePrintWriter(writer))
        e.printStackTrace()
        setOutputText(writer.toString) }).get
  }


  def initialise() {
    val panel = new JPanel()
    val layout = new GroupLayout(panel)
    panel.setLayout(layout)
    contents = Component.wrap(panel)

    val equationLayout = new SwingEquationLayout(panel)

    val chooseInput = equationLayout.addComponent(getChooseInput)
    val chooseCompile = equationLayout.addComponent(getChooseCompile)
    val chooseOutput = equationLayout.addComponent(getChooseOutput)
    val executeButton = equationLayout.addComponent(new ExecuteButton(this))
    val inputPanel = equationLayout.addComponent(getInputPanel)
    val outputPanel = equationLayout.addComponent(getOutputPanel)
    val inputGrammarButton = equationLayout.addComponent(new ShowInputGrammarButton(this))
    val outputGrammarButton = equationLayout.addComponent(new ShowOutputGrammarButton(this))
    val exampleDropdown = equationLayout.addComponent(new ExampleDropdown(this))

    val innerLayout = equationLayout.equationLayout

    equationLayout.makePreferredSize(chooseCompile)
    equationLayout.makePreferredSize(chooseOutput)
    equationLayout.makePreferredSize(chooseInput)
    equationLayout.makePreferredSize(inputGrammarButton)
    equationLayout.makePreferredSize(outputGrammarButton)
    equationLayout.makePreferredSize(exampleDropdown)
    equationLayout.makePreferredSize(executeButton)

    def addHorizontalEquations() {
      innerLayout.addLeftToRight(innerLayout.container, inputPanel, outputPanel, innerLayout.container)
      innerLayout.addLeftToRight(exampleDropdown, inputGrammarButton, outputGrammarButton, innerLayout.container)
      innerLayout.addLeftToRight(innerLayout.container, chooseInput, chooseCompile, chooseOutput, executeButton)

      innerLayout.expressions += inputPanel.width - outputPanel.width
    }
    addHorizontalEquations()

    def addVerticalEquations() {
      innerLayout.addEquals(chooseInput.verticalCenter2,
        chooseCompile.verticalCenter2,
        chooseOutput.verticalCenter2,
        inputGrammarButton.verticalCenter2,
        outputGrammarButton.verticalCenter2,
        exampleDropdown.verticalCenter2,
        executeButton.verticalCenter2)

      innerLayout.addRow(inputPanel, outputPanel)
      innerLayout.addTopToBottom(innerLayout.container, chooseInput, inputPanel, innerLayout.container)
    }
    addVerticalEquations()
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
    outputTextArea.setAutoscrolls(false)
    outputTextArea.setFont(StyleSheet.codeFont)
    outputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())

    outputTextArea.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.NEVER_UPDATE)

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
    chooseCompile.add(new JLabel("Action:"))
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
