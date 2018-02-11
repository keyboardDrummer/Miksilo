package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.language.node.GrammarKey
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.javac.methods.VariableDelta
import deltas.javac.methods.VariableDelta.{VariableKey, VariableNameKey}

object ThisVariable extends DeltaWithGrammar
{
  object Grammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val variable = find(VariableDelta.VariableGrammar)
    val thisGrammar = create(Grammar, ("this" ~> value("this").as(VariableNameKey)).asNode(VariableKey))
    variable.addOption(thisGrammar)
  }

  override def dependencies: Set[Contract] = Set(VariableDelta)

  override def description: String = "Enables using the 'this' qualifier to refer to the current instance."
}
