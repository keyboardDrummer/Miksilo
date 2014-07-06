package transformations.ssm

import core.transformation.{TransformationManager, TransformationState, MetaObject, ProgramTransformation}
import org.junit.Test
import typed.languages.ssm.SSMMachine
import org.scalatest.junit.AssertionsForJUnit._
import transformations.ssm
import SSM._
import AddStatementToSSM._

object AddIfElse extends ProgramTransformation{
  val _if = "if"
  val condition = "condition"
  val _then = "then"
  val _else = "else"
  
  def transform(program: MetaObject, state: TransformationState)
  {
    AddStatementToSSM.getStatementToLines(state).put(_if,(_if : MetaObject) => {
      val guid = state.getGUID
      val elseLabel = "elseStart" + guid
      val endLabel = "ifEnd" + guid
      val elseLabelInstruction = createLabel(elseLabel)
      val condition = _if(AddIfElse.condition).asInstanceOf[MetaObject]
      val _then = _if(AddIfElse._then).asInstanceOf[MetaObject]
      val _else = _if(AddIfElse._else).asInstanceOf[MetaObject]
      val jumpEnd = jumpAlways(endLabel)
      val jumpElse = jumpOnFalse(elseLabel)
      val endLabelInstruction = createLabel(endLabel)
      val statements = Seq(condition, jumpElse, _then, jumpEnd, elseLabelInstruction, _else, endLabelInstruction)
      statements.flatMap(statement => convertStatement(statement,state))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)

  def createIfElse(condition: MetaObject, _then: MetaObject, _else: MetaObject) = new MetaObject(_if)
  {
    data.put(AddIfElse.condition,condition)
    data.put(AddIfElse._then,_then)
    data.put(AddIfElse._else,_else)
  }
}

class TestIfElse
{
  @Test
  def testThen()
  {
    val condition = loadTrue()
    val _then = loadConstant(5)
    val _else = loadConstant(9)
    val _if = AddIfElse.createIfElse(condition,_then,_else)
    val compiler = TransformationManager.buildCompiler(Seq(AddIfElse, AddStatementToSSM))
    compiler.compile(_if)
    val typedSSM = SSM.toTyped(_if)
    val machine = new SSMMachine(typedSSM)
    machine.run()
    assertResult(5)(machine.pop())
  }

  @Test
  def testElse()
  {
    val condition = loadFalse()
    val _then = loadConstant(5)
    val _else = loadConstant(9)
    val _if = AddIfElse.createIfElse(condition,_then,_else)
    val compiler = TransformationManager.buildCompiler(Seq(AddIfElse, AddStatementToSSM))
    compiler.compile(_if)
    val typedSSM = ssm.SSM.toTyped(_if)
    val machine = new SSMMachine(typedSSM)
    machine.run()
    assertResult(9)(machine.pop())
  }
}
