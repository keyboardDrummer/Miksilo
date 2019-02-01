package deltas

import core.deltas.{Contract, Delta}
import core.language.Language
import core.smarts.SolveConstraintsDelta

object RemovePhasesAfterSolveConstraints extends Delta {
  override def description: String = "Remove phases after solve constraints"

  override def dependencies: Set[Contract] = Set.empty

  override def inject(language: Language): Unit = {
    val bla = List.empty
    val constraintIndex = language.compilerPhases.indexWhere(p => p.key == SolveConstraintsDelta)
    language.compilerPhases = language.compilerPhases.take(constraintIndex + 1)
    super.inject(language)
  }

}
