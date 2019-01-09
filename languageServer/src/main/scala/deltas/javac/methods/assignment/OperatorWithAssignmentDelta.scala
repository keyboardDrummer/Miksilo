package deltas.javac.methods.assignment

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar, DeltaWithPhase}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.expression.additive.AdditionDelta

trait OperatorWithAssignmentDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(SimpleAssignmentDelta)

  def incrementAssignment(target: Node, value: Node) =
    new Node(Shape, SimpleAssignmentDelta.Target -> target, SimpleAssignmentDelta.Value -> value)

  def keyword: String
  def operatorShape: NodeShape

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val assignmentGrammar = find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = find(SimpleAssignmentDelta.Target)
    val incrementAssignmentGrammar = assignmentTarget.as(SimpleAssignmentDelta.Target) ~~
      (keyword ~~> assignmentGrammar.as(SimpleAssignmentDelta.Value)) asNode Shape
    assignmentGrammar.addAlternative(incrementAssignmentGrammar)
  }

  def transformAssignment(incrementAssignment: NodePath, state: Language): Unit = {
    val target = SimpleAssignmentDelta.getTarget(incrementAssignment)
    val newValue = operatorShape.createWithSource(
      AdditionDelta.Left -> incrementAssignment.current(SimpleAssignmentDelta.Target),
      AdditionDelta.Right -> incrementAssignment.getWithSource(SimpleAssignmentDelta.Value))
    val assignment = SimpleAssignmentDelta.neww(target, newValue)
    incrementAssignment.replaceData(assignment)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, obj => transformAssignment(obj, compilation))
  }

  object Shape extends NodeShape
}
