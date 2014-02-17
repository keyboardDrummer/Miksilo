package languages.bytecode

import transformation.{ProgramTransformation, TransformationState, MetaObject}
import JavaBase._
import languages.ssm.AddStatementToSSM

object TernaryC extends ProgramTransformation {

  def falseBranch(metaObject: MetaObject) = metaObject("false").asInstanceOf[MetaObject]

  def trueBranch(metaObject: MetaObject) = metaObject("true").asInstanceOf[MetaObject]

  val conditionKey = "condition"
  val ternary = "ternary"
  def getCondition(metaObject: MetaObject) = {
    metaObject(conditionKey).asInstanceOf[MetaObject]
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(ternary,(_ternary : MetaObject, compiler) => {
      val condition = TernaryC.getCondition(_ternary)
      val truePath = TernaryC.trueBranch(_ternary)
      val falsePath = TernaryC.falseBranch(_ternary)
      val falseLabelName = state.getGUID.toString
      val falseTarget = ByteCodeGoTo.label(falseLabelName)
      val jump = ByteCodeGoTo.integerCompareGreater(falseLabelName)
      statementToInstructions(condition, compiler) ++
        Seq(jump) ++
        statementToInstructions(truePath, compiler) ++
        Seq(falseTarget) ++
        statementToInstructions(falsePath, compiler)
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(JavaBase)
}
