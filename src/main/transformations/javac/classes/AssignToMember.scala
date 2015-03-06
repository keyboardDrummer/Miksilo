package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.SwapInstruction
import transformations.bytecode.coreInstructions.objects.PutField
import transformations.javac.classes.SelectorC._
import transformations.javac.expressions.ExpressionC
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentC

object AssignToMember extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(AssignmentC, SelectorC)

  override def inject(state: TransformationState): Unit = {
    AssignmentC.getState(state).assignFromStackByteCodeRegistry.put(SelectorC.SelectorKey, (selector: MetaObject) => {
      val compiler = ClassC.getClassCompiler(state)
      val classOrObjectReference = getClassOrObjectReference(selector, compiler)
      val fieldRefIndex = getFieldRefIndex(selector, compiler, classOrObjectReference)

      val _object = getSelectorObject(selector)
      val objectInstructions = ExpressionC.getToInstructions(state)(_object)
      objectInstructions ++ Seq(SwapInstruction.swap, PutField.putField(fieldRefIndex))
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
