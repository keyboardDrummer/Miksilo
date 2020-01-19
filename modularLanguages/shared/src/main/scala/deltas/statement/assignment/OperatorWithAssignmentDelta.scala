package deltas.statement.assignment

import core.bigrammar.grammars.BiSequence
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar, DeltaWithPhase}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.expression.{BinaryOperatorDelta, LeftAssociativeBinaryOperatorDelta}

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
    PathRoot.fromCompilation(compilation).visitShape(shape, obj => transformAssignment(obj, compilation))
  }
}
