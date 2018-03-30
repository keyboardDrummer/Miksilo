package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.JButton

class ExecuteButton(sandbox: LanguageSandbox) extends JButton("Execute") {

  addActionListener((e: ActionEvent) => {
    sandbox.execute(() => executeClicked())
  })

  def executeClicked(): Unit = {
    val input = sandbox.inputOption.getInput
    val output = sandbox.compileOption.run(sandbox, input)
    sandbox.outputOption.handleOutput(output)
  }
}
