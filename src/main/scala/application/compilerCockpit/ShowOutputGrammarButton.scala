package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

import core.bigrammar.PrintBiGrammar
import core.deltas.Language

class ShowOutputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show output grammar") {
  addActionListener((e: ActionEvent) => {
    val deltas = compilerCockpit.deltas.dropWhile(p => p != MarkOutputGrammar)
    val language = new Language(deltas)
    val grammarString = PrintBiGrammar.toTopLevelDocument(language.grammars.root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
