package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton
import core.grammar.PrintGrammar
import core.transformation.grammars.GrammarNotFoundException

class ShowInputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show input grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      try {
        compilerCockpit.execute(Seq(GrammarToOutput, compilerCockpit.textAreaOutput), Seq())
      }
      catch {
        case e: GrammarNotFoundException =>
          val keyName = PrintGrammar.grammarKeyToName(e.key)
          compilerCockpit.setOutputText(s"Error occurred while constructing input grammar, key $keyName not found")
      }
    }
  })
}
