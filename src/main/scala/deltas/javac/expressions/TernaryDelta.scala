package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.types.BooleanTypeC

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

  override def getType(_ternary: Path, compilation: Compilation): Node = {
    val getExpressionType = ExpressionSkeleton.getType(compilation)
    val condition = TernaryDelta.getCondition(_ternary)
    val truePath = TernaryDelta.trueBranch(_ternary)
    val falsePath = TernaryDelta.falseBranch(_ternary)
    TypeSkeleton.checkAssignableTo(compilation)(BooleanTypeC.booleanType, getExpressionType(condition))

    val trueType = getExpressionType(truePath)
    val falseType = getExpressionType(falsePath)
    TypeSkeleton.union(compilation)(trueType, falseType)
  }

  override def toByteCode(_ternary: Path, compilation: Compilation): Seq[Node] = {
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
}
