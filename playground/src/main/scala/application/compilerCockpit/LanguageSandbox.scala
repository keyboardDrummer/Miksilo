package application.compilerCockpit

import java.awt._
import java.io.{ByteArrayInputStream, CharArrayWriter}
import java.nio.charset.StandardCharsets

import application.StyleSheet
import core.bigrammar.BiGrammar
import core.deltas.{Delta, LanguageFromDeltas}
import core.language.exceptions.CompileException
import core.layouts.{EquationLayout, Expression, SwingEquationLayout}
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener, ListDataEvent, ListDataListener}
import javax.swing.text.DefaultCaret
import org.fife.ui.rsyntaxtextarea._
import org.fife.ui.rtextarea.RTextScrollPane

import scala.swing.{Component, Frame}
import scala.tools.nsc.NewLinePrintWriter
import scala.util.Try

class LanguageSandbox(val name: String, val deltas: Seq[Delta],
                      presentationMode: Boolean = StyleSheet.presentationMode)
  extends Frame {
  this.title = name
  val language = LanguageFromDeltas(deltas)

  private val inputPanel = new EditorFromLanguage(language)
  private def getInputStream = () => {
    new ByteArrayInputStream(inputPanel.inputDocument.getText(0, inputPanel.inputDocument.getLength).getBytes(StandardCharsets.UTF_8))
  }
  initializeTextArea(inputPanel.inputTextArea)

  private val outputDocument = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  private val textAreaInput: InputOption = new ParseFromFunction(getInputStream)

  val textAreaOutput: TextAreaOutput =
    new TextAreaOutput(textWithGrammar => setOutputText(textWithGrammar.text, textWithGrammar.grammar))
  private val compileOptions = getCompileOptions.toArray

  def getCompileOptions: Seq[CompileOption] = {
    val byteCodeActions = Seq(CompileAndRunOption, EmitByteCode)
    val result: Seq[CompileOption] = Seq(PrettyPrintOption) ++ byteCodeActions ++ Seq(FormatOption, FormatJsonOption) //TODO enable these again. ++ language.extraCompileOptions
    result.foreach(option => option.initialize(this))
    result
  }

  private val outputOptions = Array[OutputOption](textAreaOutput)
  private val inputOptions = Array[InputOption](textAreaInput)

  var inputOptionModel = new DefaultComboBoxModel[InputOption](inputOptions)
  inputOptionModel.setSelectedItem(textAreaInput)

  val outputOptionModel = new DefaultComboBoxModel[OutputOption](outputOptions)
  outputOptionModel.setSelectedItem(textAreaOutput)

  val compileOptionModel = new DefaultComboBoxModel[CompileOption](compileOptions)
  compileOptionModel.setSelectedItem(compileOptions(0))

  val panelOptions = Array[PanelMode](Both,Input, Output)
  var panelsOptionModel = new DefaultComboBoxModel[PanelMode](panelOptions)
  panelsOptionModel.setSelectedItem(Both)

  initialise()

  def outputOption = outputOptionModel.getSelectedItem.asInstanceOf[OutputOption]

  def compileOption: CompileOption = compileOptionModel.getSelectedItem.asInstanceOf[CompileOption]

  def inputOption = inputOptionModel.getSelectedItem.asInstanceOf[InputOption]

  def setOutputText(text: String, useThisGrammar: BiGrammar = null) {
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
    inputPanel.inputDocument.replace(0, inputPanel.inputDocument.getLength, text, null)
  }

  private var executeOnChanged = false
  def toggleExecuteOnChanged(): Unit = {
    executeOnChanged = !executeOnChanged
  }
  inputPanel.inputDocument.addDocumentListener(new DocumentListener {
    override def insertUpdate(e: DocumentEvent): Unit = if (executeOnChanged) executeClicked()

    override def removeUpdate(e: DocumentEvent): Unit = if (executeOnChanged) executeClicked()

    override def changedUpdate(e: DocumentEvent): Unit = if (executeOnChanged) executeClicked()
  })

  def executeClicked(): Unit = {
    val input = inputOption.getInput
    val output = compileOption.run(this, input)
    outputOption.handleOutput(output)
  }

  def execute(action: () => Unit): Unit = {
    Try[Unit](action()).
      recover({ case e: CompileException => setOutputText(e.toString) }).
      recover({ case e: Throwable =>
        val writer = new CharArrayWriter()
        e.printStackTrace(new NewLinePrintWriter(writer))
        e.printStackTrace()
        setOutputText(writer.toString) }).get
  }

  trait PanelMode
  object Both extends PanelMode
  {
    override def toString = "Both"
  }
  object Input extends PanelMode
  {
    override def toString = "Input"
  }
  object Output extends PanelMode
  {
    override def toString = "Output"
  }


  def applyInputOutputMode(equations: Map[PanelMode, Expression], layout: EquationLayout, mode: PanelMode) = {
    for(expression <- equations.values)
    {
      layout.expressions -= expression
    }
    layout.expressions += equations(mode)
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
    val choosePanels = equationLayout.addComponent(getChoosePanels)
    val executeButton = equationLayout.addComponent(new ExecuteButton(this))
    val executeCheckBox = equationLayout.addComponent(new ExecuteCheckBox(this))
    val inputPanelComponent = equationLayout.addComponent(inputPanel)
    val outputPanel = equationLayout.addComponent(getOutputPanel)
    val showPhasesButton = equationLayout.addComponent(new ShowPhasesButton(this))
    val inputGrammarButton = equationLayout.addComponent(new ShowInputGrammarButton(this))
    val outputGrammarButton = equationLayout.addComponent(new ShowOutputGrammarButton(this))
    val exampleDropDown = equationLayout.addComponent(new ExampleDropdown(this))

    val innerLayout = equationLayout.equationLayout

    equationLayout.makePreferredSize(chooseCompile)
    equationLayout.makePreferredSize(chooseOutput)
    equationLayout.makePreferredSize(chooseInput)
    equationLayout.makePreferredSize(choosePanels)
    equationLayout.makePreferredSize(showPhasesButton)
    equationLayout.makePreferredSize(inputGrammarButton)
    equationLayout.makePreferredSize(outputGrammarButton)
    equationLayout.makePreferredSize(exampleDropDown)
    equationLayout.makePreferredSize(executeButton)
    equationLayout.makePreferredSize(executeCheckBox)

    def addHorizontalEquations() {
      innerLayout.addLeftToRight(innerLayout.container, inputPanelComponent, outputPanel, innerLayout.container)
      innerLayout.addLeftToRight(exampleDropDown, showPhasesButton, inputGrammarButton, outputGrammarButton, innerLayout.container)
      innerLayout.addLeftToRight(innerLayout.container, chooseInput, chooseCompile, chooseOutput, executeButton, executeCheckBox, choosePanels)
    }
    addHorizontalEquations()

    val inputOutputModeEquations: Map[PanelMode, Expression] = Map(Both -> inputPanelComponent.width.-(outputPanel.width),
      Input -> outputPanel.width,
      Output -> inputPanelComponent.width)

    panelsOptionModel.addListDataListener(new ListDataListener {

      override def intervalRemoved(e: ListDataEvent): Unit = {

      }

      override def intervalAdded(e: ListDataEvent): Unit = {

      }

      override def contentsChanged(e: ListDataEvent): Unit = {
        applyInputOutputMode(inputOutputModeEquations, innerLayout, panelsOptionModel.getSelectedItem.asInstanceOf[PanelMode])
        panel.revalidate()
      }
    })
    applyInputOutputMode(inputOutputModeEquations, innerLayout, Both)

    def addVerticalEquations() {
      innerLayout.addEquals(chooseInput.verticalCenter2,
        chooseCompile.verticalCenter2,
        chooseOutput.verticalCenter2,
        choosePanels.verticalCenter2,
        showPhasesButton.verticalCenter2,
        inputGrammarButton.verticalCenter2,
        outputGrammarButton.verticalCenter2,
        exampleDropDown.verticalCenter2,
        executeButton.verticalCenter2,
        executeCheckBox.verticalCenter2)

      innerLayout.addRow(inputPanelComponent, outputPanel)
      innerLayout.addTopToBottom(innerLayout.container, chooseCompile, inputPanelComponent, innerLayout.container)
    }
    addVerticalEquations()
    panel.setLayout(equationLayout)
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
    if (!presentationMode)
    {
      chooseInput.add(new JLabel("Input:"))
      val inputComboBox: JComboBox[InputOption] = new JComboBox(inputOptionModel)
      chooseInput.add(inputComboBox)
    }
    chooseInput
  }

  def getChoosePanels = {
    val choosePanels = new JPanel()
    if (!presentationMode)
      choosePanels.add(new JLabel("Panels:"))
    val inputComboBox: JComboBox[PanelMode] = new JComboBox(panelsOptionModel)
    choosePanels.add(inputComboBox)
    choosePanels
  }

  def getChooseCompile: JPanel = {
    val chooseCompile = new JPanel()
    if (!presentationMode)
      chooseCompile.add(new JLabel("Action:"))
    val compileComboBox: JComboBox[CompileOption] = new JComboBox(compileOptionModel)
    chooseCompile.add(compileComboBox)
    chooseCompile
  }

  def getChooseOutput: JPanel = {
    val chooseOutput = new JPanel()
    if (!presentationMode) {
      chooseOutput.add(new JLabel("Output:"))
      val chooseOutputComboBox = new JComboBox(outputOptionModel)
      chooseOutput.add(chooseOutputComboBox)
    }
    chooseOutput
  }
}
