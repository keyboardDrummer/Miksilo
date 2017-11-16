package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import core.deltas.Phase

class ShowPhasesButton(compilerCockpit: CompilerCockpit) extends JButton("Show phases") {
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val state = compilerCockpit.compiler.buildLanguage
      val text: String = state.compilerPhases.zipWithIndex.
        map(indexedPhase => getIndexedPhaseDescription(indexedPhase)).
        reduce((a,b) => a + "\n\n" + b)
      compilerCockpit.setOutputText(text)
    }
  })

  def getIndexedPhaseDescription(indexedPhase: (Phase, Int)): String = {
    val phase = indexedPhase._1
    val index = indexedPhase._2
    (index + 1) + ": " + phase.name.capitalize + "\n" + phase.description
  }
}
