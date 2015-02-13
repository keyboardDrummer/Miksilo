package application.compilerCockpit

class TextAreaOutput(setText: String => Unit) extends OutputOption {

  override def handleOutput(output: String): Unit = {
    setText(output)
  }

  override def toString = "Output to text area"
}
