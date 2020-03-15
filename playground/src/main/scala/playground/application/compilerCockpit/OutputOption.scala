package playground.application.compilerCockpit

import core.language.Language

object OutputOption {

  object Output

  def getOutput(state: Language) = state.data.get(Output).collect({ case x: String => x})

  def setOutput(state: Language, value: String): Unit = state.data(Output) = value
}

trait OutputOption {
    def handleOutput(output: TextWithGrammar): Unit
}
