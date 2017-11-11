package transformations.javac.expressions

import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node._
import core.particles.path.Path
import transformations.bytecode.ByteCodeMethodInfo
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.bytecode.types.TypeSkeleton
import transformations.javac.types.BooleanTypeC

object TernaryC extends ExpressionInstance {
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

  object TernaryKey extends NodeClass

  object TernaryExpressionGrammar extends GrammarKey

  override val key = TernaryKey

  override def getType(_ternary: Path, compilation: Compilation): Node = {
    val getExpressionType = ExpressionSkeleton.getType(compilation)
    val condition = TernaryC.getCondition(_ternary)
    val truePath = TernaryC.trueBranch(_ternary)
    val falsePath = TernaryC.falseBranch(_ternary)
    TypeSkeleton.checkAssignableTo(compilation)(BooleanTypeC.booleanType, getExpressionType(condition))

    val trueType = getExpressionType(truePath)
    val falseType = getExpressionType(falsePath)
    TypeSkeleton.union(compilation)(trueType, falseType)
  }

  override def toByteCode(_ternary: Path, compilation: Compilation): Seq[Node] = {
    val condition = TernaryC.getCondition(_ternary)
    val truePath = TernaryC.trueBranch(_ternary)
    val falsePath = TernaryC.falseBranch(_ternary)
    val methodInfo = _ternary.findAncestorClass(ByteCodeMethodInfo.MethodInfoKey)
    val falseLabelName = LabelledLocations.getUniqueLabel("false", methodInfo, compilation)
    val falseTarget = InferredStackFrames.label(falseLabelName)
    val conditionalBranch = LabelledLocations.ifZero(falseLabelName)
    val endLabelName = LabelledLocations.getUniqueLabel("end", methodInfo, compilation)
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
