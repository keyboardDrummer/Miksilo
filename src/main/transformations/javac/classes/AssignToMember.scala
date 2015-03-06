package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.SelectorC._
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentC

object AssignToMember extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(AssignmentC, SelectorC)

  override def inject(state: TransformationState): Unit = {
    AssignmentC.getState(state).assignFromStackByteCodeRegistry.put(SelectorC.SelectorKey, (target: MetaObject) => {
      ???
    })
    super.inject(state)
  }
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val assignTarget = grammars.find(AssignmentC.AssignmentTargetGrammar)

    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    val selectGrammar = (variableGrammar <~ ".") ~ identifier ^^ parseMap(SelectorKey, SelectorObject, SelectorMember)
    //val selectGrammar = grammars.find(SelectorC.SelectGrammar) TODO replace two lines above with this line.
    assignTarget.addOption(selectGrammar)
  }
}
