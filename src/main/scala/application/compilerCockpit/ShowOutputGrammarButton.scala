package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

import core.bigrammar.PrintBiGrammar
import core.deltas.Delta

class ShowOutputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show output grammar") {
  addActionListener((e: ActionEvent) => {
    val deltas = compilerCockpit.deltas.dropWhile(p => p != MarkOutputGrammar)
    val language = Delta.buildLanguage(deltas)
    val grammarString = PrintBiGrammar.toTopLevelDocument(language.grammars.root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
