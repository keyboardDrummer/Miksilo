package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

class ExecuteButton(compilerCockpit: CompilerCockpit) extends JButton("Execute") {

  addActionListener((e: ActionEvent) => {
    compilerCockpit.execute(() => executeClicked())
  })

  def executeClicked(): Unit = {
    val input = compilerCockpit.inputOption.getInput
    val output = compilerCockpit.compileOption.perform(compilerCockpit, input)
    compilerCockpit.outputOption.handleOutput(output)
  }
}
