package application.compilerCockpit

import java.awt.event.ActionEvent

import javax.swing.{JButton, JCheckBox}

class ExecuteButton(sandbox: LanguageSandbox) extends JButton("Execute") {

  addActionListener((e: ActionEvent) => {
    sandbox.execute(() => sandbox.executeClicked())
  })
}

class ExecuteCheckBox(sandbox: LanguageSandbox) extends JCheckBox("Execute on keystroke") {

  addActionListener((e: ActionEvent) => {
    sandbox.toggleExecuteOnChanged()
  })
}
