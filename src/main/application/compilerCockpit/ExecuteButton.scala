package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

import application.StyleSheet

class ExecuteButton(compilerCockpit: CompilerCockpit) extends JButton("EXECUTE") {

  setFont(StyleSheet.hugeFont)
  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val inputParticle = Seq(compilerCockpit.inputOption)
      val outputParticle = Seq(compilerCockpit.compileOption, compilerCockpit.outputOption)
      compilerCockpit.execute(inputParticle, outputParticle)
    }
  })

}
