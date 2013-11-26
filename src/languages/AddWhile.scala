package languages

import transformation.{MetaObject, TransformationState, ProgramTransformation}
import SSM._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

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
      val jumpEnd = jumpOnFalse(endLabel)
      val body = _while(AddWhile.body).asInstanceOf[MetaObject]
      val jumpStart = jumpAlways(startLabel)
      val endLabelInstruction = createLabel(endLabel)
      Seq.apply(startLabelInstruction, condition, jumpEnd, body, jumpStart, endLabelInstruction).flatMap(statement => AddStatementToSSM.convertStatement(statement,state))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)
}

class TestWhile
{
  @Test
  def testWhile()
  {

  }
}
