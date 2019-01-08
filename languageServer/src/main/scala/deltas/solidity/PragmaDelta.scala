package deltas.solidity

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.RegexGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.expression.ExpressionDelta

object PragmaDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Name extends NodeField
  object Values extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar: BiGrammar = "pragma" ~~ identifier.as(Name) ~~ (RegexGrammar("""[^;]+""".r) | expression) ~ ";"
    find(SolidityFile.Members).addAlternative(grammar)
  }

  override def description = "Adds pragmas"

  override def dependencies = Set(SolidityFile)
}
