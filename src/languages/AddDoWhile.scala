package languages

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.SSM._

object AddDoWhile extends ProgramTransformation {
  val body = "body"
  val condition = "condition"
  val _while = "doWhile"

  def transform(program: MetaObject, state: TransformationState) = {
    AddStatementToSSM.getStatementToLines(state).put(_while,(_while : MetaObject) => {
      val guid = state.getGUID
      val startLabel = "whileStart" + guid
      val startLabelInstruction = createLabel(startLabel)
      val condition = _while(AddWhile.condition).asInstanceOf[MetaObject]
      val body = _while(AddWhile.body).asInstanceOf[MetaObject]
      val jumpStart = jumpOnTrue(startLabel)
      Seq.apply(startLabelInstruction, body, condition, jumpStart).flatMap(statement => AddStatementToSSM.convertStatement(statement,state))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)
}
