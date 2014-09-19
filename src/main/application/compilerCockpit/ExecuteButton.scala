package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

class ExecuteButton(compilerCockpit: CompilerCockpit) extends JButton("Execute") {

  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val inputParticle = Seq(compilerCockpit.inputOption)
      val outputParticle = Seq(compilerCockpit.compileOption, compilerCockpit.outputOption)
      compilerCockpit.execute(inputParticle, outputParticle)
    }
  })

}
