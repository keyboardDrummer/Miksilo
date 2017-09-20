package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles._
import core.particles.path.Path
import transformations.bytecode.coreInstructions.SwapInstruction
import transformations.bytecode.coreInstructions.objects.PutField
import transformations.javac.classes.SelectField._
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.MemberSelector.{SelectorMember, SelectorObject, SelectorKey}
import transformations.javac.methods.{MemberSelector, VariableC}
import transformations.javac.methods.assignment.AssignmentSkeleton

object AssignToMember extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, SelectField)

  override def inject(state: CompilationState): Unit = {
    AssignmentSkeleton.getState(state).assignFromStackByteCodeRegistry.put(MemberSelector.SelectorKey, (selector: Path) => {
      val compiler = JavaClassSkeleton.getClassCompiler(state)
      val classOrObjectReference = MemberSelector.getClassOrObjectReference(selector, compiler)
      val fieldRefIndex = getFieldRef(selector, compiler, classOrObjectReference)

      val _object = MemberSelector.getSelectorObject(selector)
      val objectInstructions = ExpressionSkeleton.getToInstructions(state)(_object)
      objectInstructions ++ Seq(SwapInstruction.swap, PutField.putField(fieldRefIndex))
    })
    super.inject(state)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val assignTarget = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)

    val variableGrammar = grammars.find(VariableC.VariableGrammar)
    val selectGrammar = ((variableGrammar <~ ".") ~ identifier).asNode(SelectorKey, SelectorObject, SelectorMember)
    //val selectGrammar = grammars.find(SelectorC.SelectGrammar) TODO replace two lines above with this line.
    assignTarget.addOption(selectGrammar)
  }

  override def description: String = "Enables assignment to a field."
}
