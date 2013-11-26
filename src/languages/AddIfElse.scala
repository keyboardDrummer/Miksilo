package languages

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.SSM._
import languages.AddStatementToSSM._

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
      val ifElseBlock = createBlock(condition, jumpElse, jumpEnd, _then, jumpEnd, elseLabelInstruction, _else, endLabelInstruction)
      AddStatementToSSM.convertStatement(ifElseBlock, state)
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)
}
