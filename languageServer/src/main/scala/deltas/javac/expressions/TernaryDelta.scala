package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.bytecode.types.TypeSkeleton
import deltas.expression.ExpressionDelta
import deltas.javac.types.BooleanTypeDelta

object TernaryDelta extends DeltaWithGrammar with ExpressionInstance with ConvertsToByteCodeDelta {
  def falseBranch[T <: NodeLike](metaObject: T) = metaObject(FalseBranch).asInstanceOf[T]

  def trueBranch[T <: NodeLike](metaObject: T) = metaObject(TrueBranch).asInstanceOf[T]

  def getCondition[T <: NodeLike](metaObject: T) = {
    metaObject(Condition).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta, LabelledLocations)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val parseTernary = (expressionGrammar.as(Condition) ~~< "?") ~~
      (expressionGrammar.as(TrueBranch) ~~< ":") ~~
      expressionGrammar.as(FalseBranch) asNode Shape
    expressionGrammar.addAlternative(parseTernary)
  }

  def ternary(condition: Node, trueBranch: Node, falseBranch: Node) = new Node(Shape,
    FalseBranch -> falseBranch,
    TrueBranch -> trueBranch,
    Condition -> condition)

  object FalseBranch extends NodeField

  object TrueBranch extends NodeField

  object Condition extends NodeField

  object Shape extends NodeShape

  override val shape = Shape

  override def getType(_ternary: NodePath, compilation: Compilation): Node = {
    val getExpressionType = ExpressionDelta.getType(compilation)
    val condition = TernaryDelta.getCondition(_ternary)
    val truePath = TernaryDelta.trueBranch(_ternary)
    val falsePath = TernaryDelta.falseBranch(_ternary)
    TypeSkeleton.checkAssignableTo(compilation)(BooleanTypeDelta.booleanType, getExpressionType(condition))

    val trueType = getExpressionType(truePath)
    val falseType = getExpressionType(falsePath)
    TypeSkeleton.union(compilation)(trueType, falseType)
  }

  override def toByteCode(_ternary: NodePath, compilation: Compilation): Seq[Node] = {
    val condition = TernaryDelta.getCondition(_ternary)
    val truePath = TernaryDelta.trueBranch(_ternary)
    val falsePath = TernaryDelta.falseBranch(_ternary)
    val methodInfo = _ternary.findAncestorShape(ByteCodeMethodInfo.Shape)
    val falseLabelName = LabelDelta.getUniqueLabel("false", methodInfo)
    val falseTarget = InferredStackFrames.label(falseLabelName)
    val conditionalBranch = LabelledLocations.ifZero(falseLabelName)
    val endLabelName = LabelDelta.getUniqueLabel("end", methodInfo)
    val end = InferredStackFrames.label(endLabelName)
    val goToEnd = LabelledLocations.goTo(endLabelName)
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    toInstructions(condition) ++
      Seq(conditionalBranch) ++
      toInstructions(truePath) ++
      Seq(goToEnd, falseTarget) ++
      toInstructions(falsePath) ++
      Seq(end)
  }

  override def description: String = "Adds the ternary operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, _ternary: NodePath, _type: Type, parentScope: Scope): Unit = {
    val condition = TernaryDelta.getCondition(_ternary)
    val truePath = TernaryDelta.trueBranch(_ternary)
    val falsePath = TernaryDelta.falseBranch(_ternary)
    val conditionType = ExpressionDelta.getType(compilation, builder, condition, parentScope)
    builder.typesAreEqual(BooleanTypeDelta.constraintType, conditionType)

    val trueType = ExpressionDelta.getType(compilation, builder, truePath, parentScope)
    val falseType = ExpressionDelta.getType(compilation, builder, falsePath, parentScope)

    builder.typesAreEqual(_type, builder.getCommonSuperType(trueType, falseType))
  }
}
