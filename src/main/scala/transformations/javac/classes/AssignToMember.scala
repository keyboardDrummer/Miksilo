package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles._
import core.particles.path.Path
import transformations.bytecode.coreInstructions.SwapInstruction
import transformations.bytecode.coreInstructions.objects.PutField
import transformations.javac.classes.SelectField._
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.MemberSelector.{Member, Target, Clazz}
import transformations.javac.methods.{MemberSelector, VariableC}
import transformations.javac.methods.assignment.AssignmentSkeleton

object AssignToMember extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, SelectField)

  override def inject(state: Language): Unit = {
    AssignmentSkeleton.getRegistry(state).assignFromStackByteCodeRegistry.put(MemberSelector.Clazz,
      (compilation: Compilation, selector: Path) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      val classOrObjectReference = MemberSelector.getClassOrObjectReference(selector, compiler)
      val fieldRefIndex = getFieldRef(selector, compiler, classOrObjectReference)

      val _object = MemberSelector.getSelectorObject(selector)
      val objectInstructions = ExpressionSkeleton.getToInstructions(compilation)(_object)
      objectInstructions ++ Seq(SwapInstruction.swap, PutField.putField(fieldRefIndex))
    })
    super.inject(state)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    import grammars._
    val assignTarget = find(AssignmentSkeleton.AssignmentTargetGrammar)

    val variableGrammar = find(VariableC.VariableGrammar)
    val selectGrammar = ((variableGrammar.as(Target) ~< ".") ~ identifier.as(Member)).asNode(Clazz)
    //val selectGrammar = grammars.find(SelectorC.SelectGrammar) TODO replace two lines above with this line.
    assignTarget.addOption(selectGrammar)
  }

  override def description: String = "Enables assignment to a field."
}
