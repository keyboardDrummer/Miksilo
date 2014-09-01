package transformations.javac.expressions

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.types.{BooleanTypeC, TypeC}

object TernaryC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    val getType = ExpressionC.getType(state)
    ExpressionC.getGetTypeRegistry(state).put(TernaryKey, _ternary => {
      val condition = TernaryC.getCondition(_ternary)
      val truePath = TernaryC.trueBranch(_ternary)
      val falsePath = TernaryC.falseBranch(_ternary)
      TypeC.checkAssignableTo(state)(BooleanTypeC.booleanType, getType(condition))

      val trueType = getType(truePath)
      val falseType = getType(falsePath)
      TypeC.union(state)(trueType, falseType)
    })
    ExpressionC.getExpressionToLines(state).put(TernaryKey, _ternary => {
      val condition = TernaryC.getCondition(_ternary)
      val truePath = TernaryC.trueBranch(_ternary)
      val falsePath = TernaryC.falseBranch(_ternary)
      val falseLabelName = state.getUniqueLabel("falseStart")
      val falseTarget = InferredStackFrames.label(falseLabelName)
      val conditionalBranch = LabelledTargets.ifZero(falseLabelName)
      val endLabelName = state.getUniqueLabel("end")
      val end = InferredStackFrames.label(endLabelName)
      val goToEnd = LabelledTargets.goTo(endLabelName)
      val toInstructions = ExpressionC.getToInstructions(state)
      toInstructions(condition) ++
        Seq(conditionalBranch) ++
        toInstructions(truePath) ++
        Seq(goToEnd, falseTarget) ++
        toInstructions(falsePath) ++
        Seq(end)
    })
  }

  def falseBranch(metaObject: MetaObject) = metaObject(FalseKey).asInstanceOf[MetaObject]

  def trueBranch(metaObject: MetaObject) = metaObject(TrueKey).asInstanceOf[MetaObject]

  def getCondition(metaObject: MetaObject) = {
    metaObject(ConditionKey).asInstanceOf[MetaObject]
  }

  override def dependencies: Set[Contract] = Set(ExpressionC, LabelledTargets)

  override def transformGrammars(grammars: GrammarCatalogue) {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val parseTernary = (expressionGrammar <~ "?") ~ (expressionGrammar <~ ":") ~ expressionGrammar ^^
      parseMap(TernaryKey, ConditionKey, TrueKey, FalseKey)
    val ternaryGrammar = grammars.create(TernaryExpressionGrammar, parseTernary | expressionGrammar.inner)
    expressionGrammar.inner = ternaryGrammar
  }

  def ternary(condition: MetaObject, trueBranch: MetaObject, falseBranch: MetaObject) = new MetaObject(TernaryKey) {
    data.put(FalseKey, falseBranch)
    data.put(TrueKey, trueBranch)
    data.put(ConditionKey, condition)
  }

  object FalseKey

  object TrueKey

  object ConditionKey

  object TernaryKey

  object TernaryExpressionGrammar

}
