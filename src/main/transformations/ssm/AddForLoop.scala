package transformations.ssm

import core.transformation._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit._
import transformations.ssm.AddBlock._
import transformations.ssm.SSM._
import typed.languages.ssm.SSMMachine

object AddForLoop extends ProgramTransformation {
  val body = "_body"
  val condition = "condition"
  val forLoop = "forLoop"
  val initialisation = "initialisation"
  val increment = "increment"

  def transform(program: MetaObject, state: TransformationState) = {
    AddStatementToSSM.getStatementToLines(state).put(forLoop, (forLoop: MetaObject) => {
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
      instructions.flatMap(statement => AddStatementToSSM.convertStatement(statement, state))
    })
  }

  override def dependencies: Set[Contract] = Set(AddStatementToSSM)

  def createForLoop(initializer: MetaObject, condition: MetaObject, increment: MetaObject, body: MetaObject) = new MetaObject(forLoop) {
    data.put(AddForLoop.initialisation, initializer)
    data.put(AddForLoop.condition, condition)
    data.put(AddForLoop.increment, increment)
    data.put(AddForLoop.body, body)
  }
}

class TestForLoop {
  @Test
  def test() {
    val loopVariableIndex = 0
    val accumulatorIndex = 1
    val initializer = createBlock(loadConstant(1), storeFreeRegister(loopVariableIndex))
    val condition = createBlock(loadFreeRegister(loopVariableIndex), loadConstant(4), lessThen)
    val increment = createBlock(loadFreeRegister(loopVariableIndex), loadConstant(1), addition, storeFreeRegister(loopVariableIndex))
    val body = createBlock(loadFreeRegister(accumulatorIndex), loadConstant(2), addition, storeFreeRegister(accumulatorIndex))
    val forLoop = AddForLoop.createForLoop(initializer, condition, increment, body)
    val compiler = new Transformer(Seq(AddForLoop, AddBlock, AddStatementToSSM))
    compiler.transform(forLoop)
    val typedSSM = SSM.toTyped(forLoop)
    val machine = new SSMMachine(typedSSM)
    machine.run()
    assertResult(6)(machine.registers(5))
  }
}

