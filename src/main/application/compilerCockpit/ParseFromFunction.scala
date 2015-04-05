package application.compilerCockpit

import java.io.InputStream

class ParseFromFunction(getText: () => InputStream) extends InputOption {

  override def getInput: InputStream = {
    getText()
  }

  override def toString = "Input from text area"
}
