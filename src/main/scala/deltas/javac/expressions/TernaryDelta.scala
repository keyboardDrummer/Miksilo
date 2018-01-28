package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.NodePath
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.types.BooleanTypeDelta

object TernaryDelta extends ExpressionInstance {
  def falseBranch[T <: NodeLike](metaObject: T) = metaObject(FalseKey).asInstanceOf[T]

  def trueBranch[T <: NodeLike](metaObject: T) = metaObject(TrueKey).asInstanceOf[T]

  def getCondition[T <: NodeLike](metaObject: T) = {
    metaObject(ConditionKey).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, LabelledLocations)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val parseTernary = (expressionGrammar.as(ConditionKey) ~~< "?") ~~
      (expressionGrammar.as(TrueKey) ~~< ":") ~~
      expressionGrammar.as(FalseKey) asNode TernaryKey
    val ternaryGrammar = create(TernaryExpressionGrammar, parseTernary | expressionGrammar.inner)
    expressionGrammar.inner = ternaryGrammar
  }

  def ternary(condition: Node, trueBranch: Node, falseBranch: Node) = new Node(TernaryKey,
    FalseKey -> falseBranch,
    TrueKey -> trueBranch,
    ConditionKey -> condition)

  object FalseKey extends NodeField

  object TrueKey extends NodeField

  object ConditionKey extends NodeField

  object TernaryKey extends NodeShape

  object TernaryExpressionGrammar extends GrammarKey

  override val key = TernaryKey

  override def getType(_ternary: NodePath, compilation: Compilation): Node = {
    val getExpressionType = ExpressionSkeleton.getType(compilation)
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
    val methodInfo = _ternary.findAncestorShape(ByteCodeMethodInfo.MethodInfoKey)
    val falseLabelName = LabelDelta.getUniqueLabel("false", methodInfo)
    val falseTarget = InferredStackFrames.label(falseLabelName)
    val conditionalBranch = LabelledLocations.ifZero(falseLabelName)
    val endLabelName = LabelDelta.getUniqueLabel("end", methodInfo)
    val end = InferredStackFrames.label(endLabelName)
    val goToEnd = LabelledLocations.goTo(endLabelName)
    val toInstructions = ExpressionSkeleton.getToInstructions(compilation)
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
    val conditionType = ExpressionSkeleton.getType(compilation, builder, condition, parentScope)
    builder.typesAreEqual(BooleanTypeDelta.constraintType, conditionType)

    val trueType = ExpressionSkeleton.getType(compilation, builder, truePath, parentScope)
    val falseType = ExpressionSkeleton.getType(compilation, builder, falsePath, parentScope)

    builder.checkSubType(_type, trueType)
    builder.checkSubType(_type, falseType)
  }
}
