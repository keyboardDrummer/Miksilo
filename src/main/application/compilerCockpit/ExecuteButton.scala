package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import application.StyleSheet
import core.modularProgram.PieceCombiner
import core.transformation.TransformationState

class ExecuteButton(compilerCockpit: CompilerCockpit) extends JButton("EXECUTE") {

  setFont(StyleSheet.hugeFont)
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val pieces = Seq(compilerCockpit.inputOption) ++ compilerCockpit.transformations ++
        Seq(compilerCockpit.compileOption, compilerCockpit.outputOption)
      val state = new TransformationState()
      PieceCombiner.combineAndExecute(state, pieces.reverse)
    }
  })
}
