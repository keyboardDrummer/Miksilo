package transformations.javac.classes

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.methods.VariableC
import transformations.javac.methods.VariableC.{VariableKey, VariableNameKey}

object ThisVariable extends GrammarTransformation
{
  object Grammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val variable = grammars.find(VariableC.VariableGrammar)
    val thisGrammar = grammars.create(Grammar, ("this" ~> produce("this")) ^^ parseMap(VariableKey, VariableNameKey))
    variable.addOption(thisGrammar)
  }

  override def dependencies: Set[Contract] = Set(VariableC) ++ super.dependencies
}
