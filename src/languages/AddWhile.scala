package languages

import transformation.{MetaObject, TransformationState, ProgramTransformation}
import SSM._

object AddWhile extends ProgramTransformation {
  val body = "body"
  val condition = "condition"
  val _while = "while"
  def transform(program: MetaObject, state: TransformationState) = {
    AddStatementToSSM.getStatementToLines(state).put(_while,(_while : MetaObject) => {
      val guid = state.getGUID
      val startLabel = "whileStart" + guid
      val endLabel = "whileEnd" + guid
      val startLabelInstruction = createLabel(startLabel)
      val condition = _while(AddWhile.condition).asInstanceOf[MetaObject]
      val jumpEnd = createInstruction("jumpOnFalse", endLabel)
      val body = _while(AddWhile.body).asInstanceOf[MetaObject]
      val jumpStart = createInstruction("jumpAlways", startLabel)
      val endLabelInstruction = createLabel(endLabel)
      Seq.apply[MetaObject](startLabelInstruction, condition, jumpEnd, body, jumpStart, endLabelInstruction)
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)
}
