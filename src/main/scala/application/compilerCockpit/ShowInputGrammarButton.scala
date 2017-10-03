package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import core.grammar.PrintGrammar

class ShowInputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show input grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
        val state = compilerCockpit.compiler.buildLanguage
        val grammarString = PrintGrammar.toDocument(state.grammarCatalogue).renderString()
        compilerCockpit.setOutputText(grammarString)
    }
  })
}
