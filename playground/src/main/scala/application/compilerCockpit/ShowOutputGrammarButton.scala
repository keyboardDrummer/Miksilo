package application.compilerCockpit

import java.awt.event.ActionEvent

import core.deltas.LanguageFromDeltas
import javax.swing.JButton

class ShowOutputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show output grammar") {
  addActionListener((e: ActionEvent) => {
    val deltas = compilerCockpit.deltas.dropWhile(p => p != MarkOutputGrammar)
    val language = LanguageFromDeltas(deltas)
    val grammarString = PrintBiGrammar.toTopLevelDocument(language.grammars.root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
