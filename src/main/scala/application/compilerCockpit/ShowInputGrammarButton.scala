package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

import core.bigrammar.BiGrammarToGrammar
import core.grammar.PrintGrammar

class ShowInputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show input grammar") {
  addActionListener((e: ActionEvent) => {
    val language = compilerCockpit.language
    val grammarString = PrintGrammar.toTopLevelDocument(BiGrammarToGrammar.toGrammar(language.grammars.root)).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
