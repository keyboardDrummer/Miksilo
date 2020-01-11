package deltas.statement.assignment

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.GrammarKey
import deltas.expression.ExpressionDelta

object AssignmentPrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    val assignmentGrammar = grammars.create(AssignmentGrammar, expressionGrammar.inner)
    expressionGrammar.inner = assignmentGrammar
  }

  object AssignmentGrammar extends GrammarKey

  override def description: String = "Wraps the current expression grammar in a failing assignment grammar."
}
