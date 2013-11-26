package languages

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.SSM._

object AddForLoop extends ProgramTransformation
{
  val body = "body"
  val condition = "condition"
  val forLoop = "forLoop"
  val initialisation = "initialisation"
  val increment = "increment"
  def transform(program: MetaObject, state: TransformationState) = {
    AddStatementToSSM.getStatementToLines(state).put(forLoop,(forLoop : MetaObject) => {
      val guid = state.getGUID
      val startLabel = "forLoopStart" + guid
      val endLabel = "forLoopEnd" + guid
      val startLabelInstruction = createLabel(startLabel)
      val condition = forLoop(AddForLoop.condition).asInstanceOf[MetaObject]
      val increment = forLoop(AddForLoop.increment).asInstanceOf[MetaObject]
      val initialisation = forLoop(AddForLoop.initialisation).asInstanceOf[MetaObject]
      val jumpEnd = jumpOnFalse(endLabel)
      val body = forLoop(AddForLoop.body).asInstanceOf[MetaObject]
      val jumpStart = jumpAlways(startLabel)
      val endLabelInstruction = createLabel(endLabel)
      val instructions = Seq.apply(initialisation, startLabelInstruction, condition, jumpEnd, body, increment, jumpStart, endLabelInstruction)
      instructions.flatMap(statement => AddStatementToSSM.convertStatement(statement,state))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)
}
