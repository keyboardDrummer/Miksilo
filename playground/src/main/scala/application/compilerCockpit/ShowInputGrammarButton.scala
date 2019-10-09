package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

class ShowInputGrammarButton(compilerCockpit: LanguageSandbox) extends JButton("Show input grammar") {
  addActionListener((e: ActionEvent) => {
    val language = compilerCockpit.language
    val grammarString = PrintBiGrammar.toTopLevelDocument(language.grammars.root).renderString()
    compilerCockpit.setOutputText(grammarString)
  })
}
