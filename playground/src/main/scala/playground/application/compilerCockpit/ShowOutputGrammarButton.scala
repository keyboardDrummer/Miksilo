package playground.application.compilerCockpit

import java.awt.event.ActionEvent

import core.bigrammar.PrintBiGrammar
import core.deltas.LanguageFromDeltas
import core.deltas.grammars.LanguageGrammars
import javax.swing.JButton

class ShowOutputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show output grammar") {
  addActionListener((e: ActionEvent) => {
    val deltas = compilerCockpit.deltas.dropWhile(p => p != MarkOutputGrammar)
    val language = LanguageFromDeltas(deltas)
    val grammarString = PrintBiGrammar.toTopLevelDocument(LanguageGrammars.grammars.get(language).root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
