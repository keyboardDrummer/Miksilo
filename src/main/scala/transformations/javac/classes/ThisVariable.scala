package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.GrammarKey
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.javac.methods.VariableC
import transformations.javac.methods.VariableC.{VariableKey, VariableNameKey}

object ThisVariable extends DeltaWithGrammar
{
  object Grammar extends GrammarKey
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    import grammars._
    val variable = find(VariableC.VariableGrammar)
    val thisGrammar = create(Grammar, ("this" ~> value("this").as(VariableNameKey)).asNode(VariableKey))
    variable.addOption(thisGrammar)
  }

  override def dependencies: Set[Contract] = Set(VariableC) ++ super.dependencies

  override def description: String = "Enables using the 'this' qualifier to refer to the current instance."
}
