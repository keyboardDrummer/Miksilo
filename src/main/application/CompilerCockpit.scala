package application

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import core.grammar.ParseException
import core.modularProgram.PieceCombiner
import core.transformation.sillyCodePieces.Injector
import core.transformation.{MetaObject, TransformationState, TransformationsToPackrat}
import transformations.bytecode.PrintByteCode

import scala.swing.{Component, Frame}

trait InputOption extends Injector

class TextAreaInput(cockpit: CompilerCockpit) extends InputOption {

  override def enter(state: TransformationState): Unit = {}

  override def leave(state: TransformationState): Unit = {
    val inputStream = cockpit.inputTextArea.getText
    val manager = new TransformationsToPackrat()
    val parser = manager.buildParser(state.grammarCatalogue)

    val parseResult = parser(inputStream)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    state.program = parseResult.get.asInstanceOf[MetaObject]
  }

  override def toString = "TextAreaInput"
}

trait OutputOption extends Injector

class TextAreaOutput(cockpit: CompilerCockpit) extends OutputOption {

  override def enter(state: TransformationState): Unit = {}

  override def leave(state: TransformationState): Unit = {
    val bytes = PrintByteCode.getBytes(state.program, state).toArray
    cockpit.outputTextArea.setText(PrintByteCode.printBytes(bytes))
  }

  override def toString = "TextAreaOutput"
}

class CompilerCockpit(transformations: Seq[Injector]) extends Frame {
  val panel = new JPanel(new GridBagLayout())
  contents = Component.wrap(panel)

  val constraints = new GridBagConstraints()
  constraints.gridx = 0
  constraints.gridy = 0
  constraints.insets = new Insets(3,3,3,3)
  val chooseInput = new JPanel()
  chooseInput.add(new JLabel("Input:"))
  val inputComboBox: JComboBox[InputOption] = new JComboBox(Array[InputOption](new TextAreaInput(this)))
  chooseInput.add(inputComboBox)
  panel.add(chooseInput, constraints)

  object CompileOption extends Enumeration {
    type Option = Value
    val Compile, CompileAndRun = Value
  }
  val chooseCompile = new JPanel()
  chooseCompile.add(new JLabel("Compile:"))
  chooseCompile.add(new JComboBox(CompileOption.values.toArray))
  constraints.gridx = 1
  panel.add(chooseCompile, constraints)

  val chooseOutput = new JPanel()
  chooseOutput.add(new JLabel("Output:"))

  val chooseOutputComboBox = new JComboBox(Array(new TextAreaOutput(this)))
  chooseOutput.add(chooseOutputComboBox)
  constraints.gridx = 2
  panel.add(chooseOutput, constraints)

  val executeButton = new JButton("Execute")
  constraints.gridy = 1
  constraints.gridx = 1
  panel.add(executeButton, constraints)

  constraints.fill = GridBagConstraints.BOTH
  constraints.weightx = 1
  constraints.weighty = 1
  constraints.gridy = 1

  val cardLayout = new CardLayout()
  val inputPanel = new JPanel(cardLayout)
  val inputTextArea = new JTextArea()
  inputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())
  inputPanel.add(inputTextArea)

  constraints.gridx = 0
  panel.add(inputPanel, constraints)

  val cardLayout2 = new CardLayout()
  val outputPanel = new JPanel(cardLayout)
  val outputTextArea = new JTextArea()
  outputTextArea.setBorder(BorderFactory.createLoweredBevelBorder())
  outputPanel.add(outputTextArea)

  constraints.gridx = 2
  panel.add(outputPanel, constraints)

  executeButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val inputPiece = inputComboBox.getSelectedItem.asInstanceOf[Injector]
      val outputPiece = chooseOutputComboBox.getSelectedItem.asInstanceOf[Injector]
      val pieces = Seq(inputPiece) ++ transformations ++ Seq(outputPiece)
      val state = new TransformationState()
      PieceCombiner.combineAndExecute(state, pieces.reverse)
    }
  })
}
