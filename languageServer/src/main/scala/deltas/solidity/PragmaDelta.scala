package deltas.solidity

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.RegexGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object PragmaDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Values extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val grammar: BiGrammar = "pragma" ~~ identifier ~~ RegexGrammar("""[^;]+""".r) ~ ";"
    find(SolidityFile.Members).addAlternative(grammar)
  }

  override def description = "Adds pragmas"

  override def dependencies = Set(SolidityFile)
}
