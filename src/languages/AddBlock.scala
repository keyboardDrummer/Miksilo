package languages

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.AddStatementToSSM._

object AddBlock extends ProgramTransformation {
  val block = "block"
  val statements = "statements"

  def transform(program: MetaObject, state: TransformationState) = {
    val statementToSSMLines = getStatementToLines(state)
    statementToSSMLines.put(block, (block: MetaObject) => {
      block(statements).asInstanceOf[Seq[MetaObject]].flatMap(statement => statementToSSMLines(statement.clazz)(statement))
    })
  }

  def dependencies: Set[ProgramTransformation] = Set(AddStatementToSSM)
}
