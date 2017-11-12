package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import core.bigrammar.BiGrammarToGrammar
import core.grammar.PrintGrammar
import core.particles.CompilerFromDeltas

class ShowOutputGrammarButton(compilerCockpit: CompilerCockpit) extends JButton("Show output grammar") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val myParticles = compilerCockpit.particles.dropWhile(p => p != MarkOutputGrammar)
      val language = new CompilerFromDeltas(myParticles).buildLanguage
      val grammarString = PrintGrammar.toTopLevelDocument(BiGrammarToGrammar.toGrammar(language.grammars.root)).renderString()
      compilerCockpit.setOutputText(grammarString)
    }
  })
}
