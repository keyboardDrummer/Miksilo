package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

import core.bigrammar.BiGrammarToGrammar
import core.deltas.Language
import core.grammar.PrintGrammar

class ShowOutputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show output grammar") {
  addActionListener((e: ActionEvent) => {
    val deltas = compilerCockpit.deltas.dropWhile(p => p != MarkOutputGrammar)
    val language = new Language(deltas)
    val grammarString = PrintGrammar.toTopLevelDocument(BiGrammarToGrammar.toGrammar(language.grammars.root)).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
