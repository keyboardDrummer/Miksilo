package miksilo.playground.application.compilerCockpit

import java.awt.event.ActionEvent

import miksilo.modularLanguages.core.bigrammar.PrintBiGrammar
import javax.swing.JButton
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars

class ShowInputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show input grammar") {
  addActionListener((e: ActionEvent) => {
    val language = compilerCockpit.language
    val grammarString = PrintBiGrammar.toTopLevelDocument(LanguageGrammars.grammars.get(language).root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
