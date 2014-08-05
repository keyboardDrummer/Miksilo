package transformations.ssm

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.ssm.AddStatementToSSM._

object AddBlock extends ProgramTransformation {
  val block = "block"
  val statements = "statements"

  def transform(program: MetaObject, state: TransformationState) = {
    val statementToSSMLines = getStatementToLines(state)
    statementToSSMLines.put(block, (block: MetaObject) => {
      block(statements).asInstanceOf[Seq[MetaObject]].flatMap(statement => statementToSSMLines(statement.clazz)(statement))
    })
  }

  override def dependencies: Set[Contract] = Set(AddStatementToSSM)

  def createBlock(statements: MetaObject*) = {
    new MetaObject(block) {
      data.put(AddBlock.statements, statements)
    }
  }
}
