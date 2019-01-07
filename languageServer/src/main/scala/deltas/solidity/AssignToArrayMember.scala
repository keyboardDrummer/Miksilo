package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expression.ArrayAccessDelta
import deltas.javac.methods.assignment.SimpleAssignmentDelta

object AssignToArrayMember extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val assignTarget = find(SimpleAssignmentDelta.AssignmentTargetGrammar)

    val arrayMember = find(ArrayAccessDelta.Shape)
    assignTarget.addAlternative(arrayMember)
  }

  override def description = "Allows assigning to an array member"

  override def dependencies = Set(ArrayAccessDelta)
}
