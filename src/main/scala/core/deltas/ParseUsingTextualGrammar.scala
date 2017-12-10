package core.deltas

import core.deltas.node.Node

object ParseUsingTextualGrammar extends Delta {
  override def inject(language: Language): Unit = {
    language._parse = input => {
      val inputString = scala.io.Source.fromInputStream(input).mkString
      val manager = new DeltasToParserConverter()
      manager.parse(language.grammars.root, inputString).asInstanceOf[Node]
    }
  }

  override def description: String = "Parses the input file using a textual grammar."
}
