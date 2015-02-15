package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import core.grammar.PrintGrammar
import core.transformation.CompilerFromParticles

class ShowOutputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show output grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val myParticles = compilerCockpit.particles.dropWhile(p => p != MarkOutputGrammar)
      val state = new CompilerFromParticles(myParticles).buildState
      val grammarString = PrintGrammar.toDocument(state.grammarCatalogue).renderString()
      compilerCockpit.setOutputText(grammarString)
    }
  })
}
