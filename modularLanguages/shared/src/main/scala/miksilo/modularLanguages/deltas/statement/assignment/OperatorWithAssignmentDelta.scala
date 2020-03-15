package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.bigrammar.grammars.BiSequence
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar, DeltaWithPhase}
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.expression.{BinaryOperatorDelta, LeftAssociativeBinaryOperatorDelta}

trait OperatorWithAssignmentDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = s"Defines the ${operatorDelta.keyword}= operator."

  override def dependencies: Set[Contract] = Set(operatorDelta, SimpleAssignmentDelta)

  val shape: NodeShape

  def neww(target: Node, value: Node) =
    new Node(shape, SimpleAssignmentDelta.Target -> target, SimpleAssignmentDelta.Value -> value)

  def operatorDelta: BinaryOperatorDelta

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val assignmentGrammar = find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = find(SimpleAssignmentDelta.Target)
    val operatorGrammar = assignmentTarget ~~
      (leftRight(operatorDelta.keyword, "=", BiSequence.identity) ~~> assignmentGrammar.as(SimpleAssignmentDelta.Value)) asLabelledNode shape
    assignmentGrammar.addAlternative(operatorGrammar)
  }

  def transformAssignment(incrementAssignment: NodePath, state: Language): Unit = {
    val target = SimpleAssignmentDelta.getTarget(incrementAssignment)
    val newValue = operatorDelta.shape.createWithData(
      LeftAssociativeBinaryOperatorDelta.Left -> incrementAssignment.current(SimpleAssignmentDelta.Target),
      LeftAssociativeBinaryOperatorDelta.Right -> incrementAssignment.getFieldData(SimpleAssignmentDelta.Value))
    val assignment = SimpleAssignmentDelta.neww(target, newValue)
    incrementAssignment.replaceData(assignment)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(shape, obj => transformAssignment(obj, compilation))
  }
}
