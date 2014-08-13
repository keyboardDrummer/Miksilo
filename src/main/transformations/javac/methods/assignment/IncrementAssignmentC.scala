package transformations.javac.methods.assignment

import core.grammar.{Grammar, seqr}
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.expressions.additive.AdditionC
import transformations.javac.methods.VariableC
import transformations.javac.statements.StatementC

object IncrementAssignmentC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(AdditionC, AssignmentC)

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(IncrementAssignmentKey, incrementAssignment => {
      val target = incrementAssignment(TargetKey).asInstanceOf[String]
      val value = incrementAssignment(ValueKey).asInstanceOf[MetaObject]
      val newValue = AdditionC.addition(VariableC.variable(target), value)
      val assignment = AssignmentC.assignment(target, newValue)

      val toInstructions = StatementC.getToInstructions(state)
      toInstructions(assignment)
    })
  }

  def incrementAssignment(target: String, value: MetaObject) =
    new MetaObject(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val assignmentGrammar = grammars.find(AssignmentPrecedence.AssignmentGrammar)
    val incrementAssignmentGrammar: Grammar = identifier ~ ("+=" ~> assignmentGrammar) ^^
      { case target seqr value => new MetaObject(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value) }
    assignmentGrammar.orToInner(incrementAssignmentGrammar)
  }

  object IncrementAssignmentKey
  object TargetKey
  object ValueKey

}
