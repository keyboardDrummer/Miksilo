package application.compilerCockpit

import java.io.InputStream

class ParseFromFunction(getText: () => String) extends InputOption {

  override def getInput: String = {
    getText()
  }

  override def toString = "Input from text area"
}
