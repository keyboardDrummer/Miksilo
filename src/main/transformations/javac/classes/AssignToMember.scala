package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.methods.assignment.AssignmentC

object AssignToMember extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(AssignmentC, SelectorC)

  override def inject(state: TransformationState): Unit = {
    AssignmentC.getState(state).assignFromStackByteCodeRegistry.put(SelectorC.SelectorKey, (target: MetaObject) => {
      ???
    })
  }
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val assignTarget = grammars.find(AssignmentC.AssignmentTargetGrammar)
    val selectGrammar = grammars.find(SelectorC.SelectGrammar)
    assignTarget.addOption(selectGrammar)
  }
}
