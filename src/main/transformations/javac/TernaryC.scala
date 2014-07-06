package transformations.javac

import core.transformation.{ProgramTransformation, TransformationState, MetaObject}
import transformations.javac.base.JavaBase
import JavaBase._
import transformations.bytecode.{InferredStackFrames, LabelledJumps}

object TernaryC extends ProgramTransformation {

  object FalseKey
  object TrueKey
  object ConditionKey
  def ternary(condition: MetaObject, trueBranch: MetaObject, falseBranch: MetaObject) = new MetaObject(TernaryKey) {
    data.put(FalseKey, falseBranch)
    data.put(TrueKey, trueBranch)
    data.put(ConditionKey, condition)
  }

  def falseBranch(metaObject: MetaObject) = metaObject(FalseKey).asInstanceOf[MetaObject]

  def trueBranch(metaObject: MetaObject) = metaObject(TrueKey).asInstanceOf[MetaObject]

  object TernaryKey
  def getCondition(metaObject: MetaObject) = {
    metaObject(ConditionKey).asInstanceOf[MetaObject]
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(TernaryKey,(_ternary : MetaObject, compiler) => {
      val condition = TernaryC.getCondition(_ternary)
      val truePath = TernaryC.trueBranch(_ternary)
      val falsePath = TernaryC.falseBranch(_ternary)
      val falseLabelName = state.getUniqueLabel("falseStart")
      val falseTarget = InferredStackFrames.label(falseLabelName)
      val conditionalBranch = LabelledJumps.ifZero(falseLabelName)
      val endLabelName = state.getUniqueLabel("end")
      val end = InferredStackFrames.label(endLabelName)
      val goToEnd = LabelledJumps.goTo(endLabelName)
      statementToInstructions(condition, compiler) ++
        Seq(conditionalBranch) ++
        statementToInstructions(truePath, compiler) ++
        Seq(goToEnd, falseTarget) ++
        statementToInstructions(falsePath, compiler) ++
        Seq(end)
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
