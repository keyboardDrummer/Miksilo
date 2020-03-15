package playground.application.compilerCockpit

class TextAreaOutput(setText: TextWithGrammar => Unit) extends OutputOption {

  override def handleOutput(textWithGrammar: TextWithGrammar): Unit = {
    setText(textWithGrammar)
  }

  override def toString = "Output to text area"
}
