package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import core.bigrammar.BiGrammarToGrammar
import core.grammar.PrintGrammar

class ShowInputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show input grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
        val language = compilerCockpit.compiler.buildLanguage
        val grammarString = PrintGrammar.toTopLevelDocument(BiGrammarToGrammar.toGrammar(language.grammars.root)).renderString()
        compilerCockpit.setOutputText(grammarString)
    }
  })
}
