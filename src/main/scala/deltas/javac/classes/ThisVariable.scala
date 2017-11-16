package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.GrammarKey
import core.deltas.{Contract, DeltaWithGrammar, Language}
import deltas.javac.methods.VariableC
import deltas.javac.methods.VariableC.{VariableKey, VariableNameKey}

object ThisVariable extends DeltaWithGrammar
{
  object Grammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val variable = find(VariableC.VariableGrammar)
    val thisGrammar = create(Grammar, ("this" ~> value("this").as(VariableNameKey)).asNode(VariableKey))
    variable.addOption(thisGrammar)
  }

  override def dependencies: Set[Contract] = Set(VariableC) ++ super.dependencies

  override def description: String = "Enables using the 'this' qualifier to refer to the current instance."
}
