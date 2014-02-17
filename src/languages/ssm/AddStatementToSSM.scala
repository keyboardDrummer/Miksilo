package languages.ssm

import transformation.{MetaObject, TransformationState, ProgramTransformation}
import scala.collection.mutable
import SSM._

object AddStatementToSSM extends ProgramTransformation {

  def getStatementToLines(state: TransformationState) = state.data.getOrElseUpdate(this, mutable.Map.empty)
    .asInstanceOf[mutable.Map[AnyRef, MetaObject => Seq[MetaObject]]]

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

  def dependencies: Set[ProgramTransformation] = Set.empty
}
