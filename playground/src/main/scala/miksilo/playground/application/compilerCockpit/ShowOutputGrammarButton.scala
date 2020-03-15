package miksilo.playground.application.compilerCockpit

import java.awt.event.ActionEvent

import miksilo.modularLanguages.core.bigrammar.PrintBiGrammar
import javax.swing.JButton
import miksilo.modularLanguages.core.deltas.LanguageFromDeltas
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars

class ShowOutputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show output grammar") {
  addActionListener((e: ActionEvent) => {
    val deltas = compilerCockpit.deltas.dropWhile(p => p != MarkOutputGrammar)
    val language = LanguageFromDeltas(deltas)
    val grammarString = PrintBiGrammar.toTopLevelDocument(LanguageGrammars.grammars.get(language).root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
