package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.HasNameDelta
import deltas.expression.ExpressionDelta

object OperationDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val name = find(HasNameDelta.Name)
    val shapeIdentifier = find(GenericSmithyDelta.ShapeIdentifierGrammar)
    val operationErrors = "errors" ~~ "[" ~~ shapeIdentifier.manySeparated(",") ~~ "]"
    val operationResults = ("->" ~ shapeIdentifier).option ~~ operationErrors.option
    val grammar = "operation" ~~ name ~~ shapeIdentifier.manySeparated(printSpace).inParenthesis ~~ operationResults
    val members = find(ShapeStatementDelta.ShapeBody)
    members.addAlternative(grammar)
  }

  override def description = "Adds the namespace service statement"

  override def dependencies = Set(ShapeStatementDelta)
}
