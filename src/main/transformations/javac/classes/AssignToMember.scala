package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}
import transformations.bytecode.coreInstructions.SwapInstruction
import transformations.bytecode.coreInstructions.objects.PutField
import transformations.javac.classes.SelectorC._
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentSkeleton

object AssignToMember extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, SelectorC)

  override def inject(state: CompilationState): Unit = {
    AssignmentSkeleton.getState(state).assignFromStackByteCodeRegistry.put(SelectorC.SelectorKey, (selector: MetaObject) => {
      val compiler = JavaClassSkeleton.getClassCompiler(state)
      val classOrObjectReference = getClassOrObjectReference(selector, compiler)
      val fieldRefIndex = getFieldRefIndex(selector, compiler, classOrObjectReference)

      val _object = getSelectorObject(selector)
      val objectInstructions = ExpressionSkeleton.getToInstructions(state)(_object)
      objectInstructions ++ Seq(SwapInstruction.swap, PutField.putField(fieldRefIndex))
    })
    super.inject(state)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val assignTarget = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)

    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    val selectGrammar = (variableGrammar <~ ".") ~ identifier ^^ parseMap(SelectorKey, SelectorObject, SelectorMember)
    //val selectGrammar = grammars.find(SelectorC.SelectGrammar) TODO replace two lines above with this line.
    assignTarget.addOption(selectGrammar)
  }

  override def description: String = "Enables assignment to a field."
}
