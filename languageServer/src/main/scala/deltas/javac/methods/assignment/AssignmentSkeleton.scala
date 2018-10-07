package deltas.javac.methods.assignment

import core.bigrammar.grammars.BiFailure
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import deltas.bytecode.coreInstructions.{Duplicate2InstructionDelta, DuplicateInstructionDelta}
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ExpressionInstance, ToByteCodeSkeleton}
import deltas.javac.methods.MethodDelta
import deltas.bytecode.types.TypeSkeleton
import deltas.expressions.ExpressionDelta

object AssignmentSkeleton extends ExpressionInstance with ConvertsToByteCodeDelta {

  def getAssignmentTarget[T <: NodeLike](assignment: T): T = assignment(Target).asInstanceOf[T]

  def getAssignmentValue[T <: NodeLike](assignment: T): T = assignment(Value).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(MethodDelta, StoreAddressDelta, StoreIntegerDelta, AssignmentPrecedence)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val targetGrammar = create(AssignmentTargetGrammar, BiFailure())
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar) //TODO shouldn't this use AssignmentPrecedence?
    val assignmentGrammar = targetGrammar.as(Target) ~~< "=" ~~ expressionGrammar.as(Value) asNode Shape
    expressionGrammar.addAlternative(assignmentGrammar)
  }

  object AssignmentTargetGrammar extends GrammarKey

  def neww(target: Node, value: Node) = new Node(Shape, Target -> target, Value -> value)

  object Shape extends NodeShape

  object Target extends NodeField

  object Value extends NodeField

  override val shape = Shape

  override def getType(assignment: NodePath, compilation: Compilation): Node = {
    val target = getAssignmentTarget(assignment)
    ExpressionDelta.getType(compilation)(target)
  }

  trait HasAssignFromStackByteCode {
    def getAssignFromStackByteCode(compilation: Compilation, path: NodePath): Seq[Node]
  }

  val hasAssignFromStackByteCode = new ShapeProperty[HasAssignFromStackByteCode]

  override def toByteCode(assignment: NodePath, compilation: Compilation): Seq[Node] = {
    val value = getAssignmentValue(assignment)
    val valueInstructions = ToByteCodeSkeleton.getToInstructions(compilation)(value)
    val target = getAssignmentTarget(assignment)
    val assignInstructions = hasAssignFromStackByteCode(compilation, target.shape).getAssignFromStackByteCode(compilation, target)
    val valueType = ExpressionDelta.getType(compilation)(value)
    val duplicateInstruction = TypeSkeleton.getTypeSize(valueType, compilation) match
    {
      case 1 => DuplicateInstructionDelta.duplicate
      case 2 =>  Duplicate2InstructionDelta.duplicate
    }
    valueInstructions ++ Seq(duplicateInstruction) ++ assignInstructions
  }

  override def description: String = "Enables assignment to an abstract target using the = operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, assignment: NodePath, _type: Type, parentScope: Scope): Unit = {
    val value = getAssignmentValue(assignment)
    val valueType = ExpressionDelta.getType(compilation, builder, value, parentScope)
    val target = getAssignmentTarget(assignment)
    val targetType = ExpressionDelta.getType(compilation, builder, target, parentScope)
    builder.typesAreEqual(targetType, _type)
    builder.isFirstSubsetOfSecond(valueType, targetType)
  }
}
