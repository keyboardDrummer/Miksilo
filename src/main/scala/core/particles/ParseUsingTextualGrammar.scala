package core.particles

import core.particles.node.Node

object ParseUsingTextualGrammar extends Delta {
  override def inject(language: Language): Unit = {
    language.parse = input => {
      val inputString = scala.io.Source.fromInputStream(input).mkString
      val manager = new DeltasToParserConverter()
      manager.parse(language.grammars.root, inputString).asInstanceOf[Node]
    }
  }

  override def description: String = "Parses the input file using a textual grammar."
}
