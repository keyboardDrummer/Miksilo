package transformations.ssm

import core.transformation.{Contract, MetaObject, ProgramTransformation, TransformationState}
import transformations.ssm.SSM._

import scala.collection.mutable

object AddStatementToSSM extends ProgramTransformation {

  def convertStatement(statement: MetaObject, state: TransformationState) = {
    val statementToSSMLines = getStatementToLines(state)
    statementToSSMLines(statement.clazz)(statement)
  }

  def transform(program: MetaObject, state: TransformationState) = {
    val statementToSSMLines = getStatementToLines(state)
    def convertStatement(statement: MetaObject) = {
      statementToSSMLines(statement.clazz)(statement)
    }
    statementToSSMLines.put(instruction, (instruction: MetaObject) => {
      Seq(instruction)
    })
    program.data.put(lines, convertStatement(program))
  }

  def getStatementToLines(state: TransformationState) = state.data.getOrElseUpdate(this, mutable.Map.empty)
    .asInstanceOf[mutable.Map[AnyRef, MetaObject => Seq[MetaObject]]]

  override def dependencies: Set[Contract] = Set.empty
}
