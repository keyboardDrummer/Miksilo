package transformations.ssm

import core.transformation.{MetaObject, ProgramTransformation, TransformationManager, TransformationState}
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit._
import transformations.ssm
import transformations.ssm.AddBlock._
import transformations.ssm.SSM._
import typed.languages.ssm.SSMMachine

object AddWhile extends ProgramTransformation {
  val body = "_body"
  val condition = "condition"
  val _while = "while"

  def transform(program: MetaObject, state: TransformationState) = {
    AddStatementToSSM.getStatementToLines(state).put(_while, (_while: MetaObject) => {
      val guid = state.getGUID
      val startLabel = "whileStart" + guid
      val endLabel = "whileEnd" + guid
      val startLabelInstruction = createLabel(startLabel)
      val condition = _while(AddWhile.condition).asInstanceOf[MetaObject]
      val jumpEnd = jumpOnFalse(endLabel)
      val body = _while(AddWhile.body).asInstanceOf[MetaObject]
      val jumpStart = jumpAlways(startLabel)
      val endLabelInstruction = createLabel(endLabel)
      Seq.apply(startLabelInstruction, condition, jumpEnd, body, jumpStart, endLabelInstruction).flatMap(statement => AddStatementToSSM.convertStatement(statement, state))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)

  def createWhile(condition: MetaObject, body: MetaObject) = {
    new MetaObject(_while) {
      data.put(AddWhile.condition, condition); data.put(AddWhile.body, body)
    }
  }
}

class TestWhile {
  @Test
  def test() {
    val condition = createBlock(loadFreeRegister(0), loadConstant(3), notEquals)
    val body = createBlock(loadFreeRegister(0), loadConstant(1), addition, storeFreeRegister(0))
    val _while = AddWhile.createWhile(condition, body)
    val compiler = TransformationManager.buildCompiler(Seq(AddWhile, AddBlock, AddStatementToSSM))
    compiler.transform(_while)
    val typedSSM = ssm.SSM.toTyped(_while)
    val machine = new SSMMachine(typedSSM)
    machine.run()
    assertResult(3)(machine.registers(4))
  }
}
