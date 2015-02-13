package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JButton

class ExecuteButton(compilerCockpit: CompilerCockpit) extends JButton("Execute") {

  addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      compilerCockpit.execute(() => {
        val input = compilerCockpit.inputOption.getInput
        val output = compilerCockpit.compileOption.perform(compilerCockpit, input)
        compilerCockpit.outputOption.handleOutput(output)
      })
    }
  })

}
