package playground.application.compilerCockpit

import java.awt.event.ActionEvent

import miksilo.modularLanguages.core.bigrammar.PrintBiGrammar
import core.deltas.grammars.LanguageGrammars
import javax.swing.JButton

class ShowInputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show input grammar") {
  addActionListener((e: ActionEvent) => {
    val language = compilerCockpit.language
    val grammarString = PrintBiGrammar.toTopLevelDocument(LanguageGrammars.grammars.get(language).root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
