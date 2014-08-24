package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import core.grammar.ToDocument
import core.modularProgram.PieceCombiner
import core.transformation.TransformationState
import core.transformation.grammars.GrammarNotFoundException

class ShowInputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val pieces = Seq(InputGrammarToOutput, compilerCockpit.textAreaOutput) ++ compilerCockpit.transformations
      val state = new TransformationState()
      try {
        PieceCombiner.combineAndExecute(state, pieces.reverse)
      }
      catch {
        case e: GrammarNotFoundException =>
          val keyName = ToDocument.grammarKeyToName(e.key)
          compilerCockpit.setOutputText(s"Error occurred while constructing grammar, key $keyName not found")
      }
    }
  })
}
