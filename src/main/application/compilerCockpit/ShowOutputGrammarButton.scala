package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton
import core.grammar.ToDocument
import core.transformation.grammars.GrammarNotFoundException

class ShowOutputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show output grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      try {
        compilerCockpit.execute(Seq(), Seq(GrammarToOutput, compilerCockpit.textAreaOutput))
      }
      catch {
        case e: GrammarNotFoundException =>
          val keyName = ToDocument.grammarKeyToName(e.key)
          compilerCockpit.setOutputText(s"Error occurred while constructing output grammar, key $keyName not found")
      }
    }
  })
}
