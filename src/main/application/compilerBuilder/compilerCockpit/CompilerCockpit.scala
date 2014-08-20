package application.compilerBuilder.compilerCockpit

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import core.modularProgram.PieceCombiner
import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector

import scala.swing.{Component, Frame}

class CompilerCockpit(transformations: Seq[Injector]) extends Frame {
  val panel = new JPanel(new GridBagLayout())
  contents = Component.wrap(panel)

  val constraints = new GridBagConstraints()
  constraints.gridx = 0
  constraints.gridy = 0
  constraints.insets = new Insets(3, 3, 3, 3)
  val chooseInput = new JPanel()
  chooseInput.add(new JLabel("Input:"))
  val inputComboBox: JComboBox[InputOption] = new JComboBox(Array[InputOption](new TextAreaInput(this)))
  chooseInput.add(inputComboBox)
  panel.add(chooseInput, constraints)

  val chooseCompile = new JPanel()
  chooseCompile.add(new JLabel("Compile:"))
  val compileComboBox: JComboBox[CompileOption] = new JComboBox(Array(CompileByteCode, Run))
  chooseCompile.add(compileComboBox)
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
      val compilePiece = compileComboBox.getSelectedItem.asInstanceOf[Injector]
      val outputPiece = chooseOutputComboBox.getSelectedItem.asInstanceOf[Injector]
      val pieces = Seq(inputPiece) ++ transformations ++ Seq(compilePiece, outputPiece)
      val state = new TransformationState()
      PieceCombiner.combineAndExecute(state, pieces.reverse)
    }
  })
}
