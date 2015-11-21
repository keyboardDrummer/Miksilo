package application.compilerCockpit

import java.awt._
import java.io.{ByteArrayInputStream, CharArrayWriter}
import java.nio.charset.StandardCharsets
import javax.swing._
import javax.swing.text.DefaultCaret

import application.StyleSheet
import core.bigrammar.BiGrammarToGrammar
import core.grammar.Grammar
import core.layouts.SwingEquationLayout
import core.particles.exceptions.CompileException
import core.particles.{CompilerFromParticles, Particle}
import org.fife.ui.rsyntaxtextarea._
import org.fife.ui.rtextarea.RTextScrollPane
import transformations.bytecode.ByteCodeSkeleton

import scala.swing.{Component, Frame}
import scala.tools.nsc.NewLinePrintWriter
import scala.util.Try

class CompilerCockpit(val name: String, val particles: Seq[Particle]) extends Frame {

  this.title = name
  val compiler = new CompilerFromParticles(particles)
  val grammar = BiGrammarToGrammar.toGrammar(compiler.getGrammar)
  val factory = new TokenMakerFactoryFromGrammar(grammar)

  private val inputDocument = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  inputDocument.setTokenMakerFactory(factory)

  private val outputDocument = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  val textAreaInput: ParseFromFunction = new ParseFromFunction(() => {
    val stringText = inputDocument.getText(0, inputDocument.getLength)
    new ByteArrayInputStream(stringText.getBytes(StandardCharsets.UTF_8))
  })

  val textAreaOutput: TextAreaOutput =
    new TextAreaOutput(textWithGrammar => setOutputText(textWithGrammar.text, textWithGrammar.grammar))
  val compileOptions = getCompileOptions.toArray

  def getCompileOptions: Seq[CompileOption] = {
    val selection = Set(MarkOutputGrammar, ByteCodeSkeleton)
    val orderedSelection = particles.filter(o => selection.contains(o))
    val byteCodeActions = Seq(CompileAndRunOption, EmitByteCode) //if (orderedSelection.take(1) == Seq(ByteCodeSkeleton)) Seq(CompileAndRun, EmitByteCode) else Seq.empty
    byteCodeActions ++ Seq(PrettyPrintOption)
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

  def setOutputText(text: String, useThisGrammar: Grammar = null) {
    outputDocument.replace(0, outputDocument.getLength, text, null)
    if (useThisGrammar != null)
    {
      outputDocument.setSyntaxStyle(new TokenMakerFromGrammar(useThisGrammar))
    }
    else
    {
      outputDocument.setSyntaxStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
    }
  }

  def setInputText(text: String) {
    inputDocument.replace(0, inputDocument.getLength, text, null)
  }

  def execute(action: () => Unit) = {
    Try[Unit](action()).
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
    val showPhasesButton = equationLayout.addComponent(new ShowPhasesButton(this))
    val inputGrammarButton = equationLayout.addComponent(new ShowInputGrammarButton(this))
    val outputGrammarButton = equationLayout.addComponent(new ShowOutputGrammarButton(this))
    val exampleDropdown = equationLayout.addComponent(new ExampleDropdown(this))

    val innerLayout = equationLayout.equationLayout

    equationLayout.makePreferredSize(chooseCompile)
    equationLayout.makePreferredSize(chooseOutput)
    equationLayout.makePreferredSize(chooseInput)
    equationLayout.makePreferredSize(showPhasesButton)
    equationLayout.makePreferredSize(inputGrammarButton)
    equationLayout.makePreferredSize(outputGrammarButton)
    equationLayout.makePreferredSize(exampleDropdown)
    equationLayout.makePreferredSize(executeButton)

    def addHorizontalEquations() {
      innerLayout.addLeftToRight(innerLayout.container, inputPanel, outputPanel, innerLayout.container)
      innerLayout.addLeftToRight(exampleDropdown, showPhasesButton, inputGrammarButton, outputGrammarButton, innerLayout.container)
      innerLayout.addLeftToRight(innerLayout.container, chooseInput, chooseCompile, chooseOutput, executeButton)

      innerLayout.expressions += inputPanel.width - outputPanel.width
    }
    addHorizontalEquations()

    def addVerticalEquations() {
      innerLayout.addEquals(chooseInput.verticalCenter2,
        chooseCompile.verticalCenter2,
        chooseOutput.verticalCenter2,
        showPhasesButton.verticalCenter2,
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
    val inputTextArea = new RSyntaxTextArea(inputDocument)
    initializeTextArea(inputTextArea)
    panel.add(new RTextScrollPane(inputTextArea))
    panel
  }

  def getOutputPanel: JPanel = {
    val cardLayout = new CardLayout()
    val outputPanel = new JPanel(cardLayout)
    val outputTextArea = new RSyntaxTextArea(outputDocument)
    initializeTextArea(outputTextArea)

    outputTextArea.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.NEVER_UPDATE) //TODO this is used to prevent auto scrolling. this has nasty edit side-effects. find another solution.

    val scrollPane = new RTextScrollPane(outputTextArea)
    outputPanel.add(scrollPane)
    outputPanel
  }

  def initializeTextArea(inputTextArea: RSyntaxTextArea): Unit = {
    inputTextArea.setBracketMatchingEnabled(false)
    inputTextArea.setFont(StyleSheet.codeFont)
    inputTextArea.setTabSize(4)
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
