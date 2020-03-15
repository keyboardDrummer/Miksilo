package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.SwapInstruction
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.PutField
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.javac.expressions.ToByteCodeSkeleton
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta.MemberSelector
import miksilo.modularLanguages.deltas.javac.methods.{AssignmentToByteCodeDelta, MemberSelectorDelta}
import miksilo.modularLanguages.deltas.statement.assignment.SimpleAssignmentDelta

object AssignToMemberDelta extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(SimpleAssignmentDelta, SelectFieldDelta)

  override def inject(language: Language): Unit = {
    AssignmentToByteCodeDelta.hasAssignFromStackByteCode.add(language, MemberSelectorDelta.Shape,
      (compilation: Compilation, selector: NodePath) => {
      val fieldRefIndex = SelectFieldToByteCodeDelta.getFieldRefIndex(compilation, selector)

      val _object = (selector: MemberSelector[NodePath]).target
      val objectInstructions = ToByteCodeSkeleton.getToInstructions(compilation)(_object)
      objectInstructions ++ Seq(SwapInstruction.swap, PutField.putField(fieldRefIndex))
    })
    super.inject(language)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val assignTarget = find(SimpleAssignmentDelta.AssignmentTargetGrammar)

    val variableGrammar = find(VariableDelta.Shape)
    val selectGrammar = find(MemberSelectorDelta.Shape)
    assignTarget.addAlternative(selectGrammar)
  }

  override def description: String = "Enables assignment to a field."
}
