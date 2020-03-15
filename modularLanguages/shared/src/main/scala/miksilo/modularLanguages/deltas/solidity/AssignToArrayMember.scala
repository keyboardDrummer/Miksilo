package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.expression.BracketAccessDelta
import miksilo.modularLanguages.deltas.statement.assignment.SimpleAssignmentDelta

object AssignToArrayMember extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val assignTarget = find(SimpleAssignmentDelta.AssignmentTargetGrammar)

    val arrayMember = find(BracketAccessDelta.Shape)
    assignTarget.addAlternative(arrayMember)
  }

  override def description = "Allows assigning to an array member"

  override def dependencies = Set(BracketAccessDelta)
}
