package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.SwapInstruction
import deltas.bytecode.coreInstructions.objects.PutField
import deltas.expressions.VariableDelta
import deltas.javac.classes.SelectField._
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.MemberSelectorDelta.{Member, MemberSelector, Shape, Target}
import deltas.javac.methods.assignment.{EqualsAssignmentDelta, AssignmentToByteCodeDelta}

object AssignToMember extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(EqualsAssignmentDelta, SelectField)

  override def inject(language: Language): Unit = {
    AssignmentToByteCodeDelta.hasAssignFromStackByteCode.add(language, MemberSelectorDelta.Shape,
      (compilation: Compilation, selector: NodePath) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      val classOrObjectReference = MemberSelectorDelta.getClassOrObjectReference(selector, compiler)
      val fieldRefIndex = getFieldRef(selector, compiler, classOrObjectReference)

      val _object = (selector: MemberSelector[NodePath]).target
      val objectInstructions = ToByteCodeSkeleton.getToInstructions(compilation)(_object)
      objectInstructions ++ Seq(SwapInstruction.swap, PutField.putField(fieldRefIndex))
    })
    super.inject(language)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val assignTarget = find(EqualsAssignmentDelta.AssignmentTargetGrammar)

    val variableGrammar = find(VariableDelta.Shape)
    val selectGrammar = ((variableGrammar.as(Target) ~< ".") ~ identifier.as(Member)).asNode(Shape)
    //val selectGrammar = grammars.find(SelectorC.SelectGrammar) TODO replace two lines above with this line.
    assignTarget.addAlternative(selectGrammar)
  }

  override def description: String = "Enables assignment to a field."
}
