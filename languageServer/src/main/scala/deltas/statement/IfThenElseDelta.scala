package deltas.statement

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node._
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelledLocations}

object IfThenElseDelta extends DeltaWithGrammar {

  override def description: String = "Enables using the if-then-else construct."

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta, LabelledLocations, InferredStackFrames, BlockDelta)

  object Shape extends NodeShape
  object ElseKey extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = find(IfThenDelta.Shape)
    val ifThenElseGrammar = ifThenGrammar.inner.asInstanceOf[NodeGrammar].inner ~ ("else" ~> bodyGrammar.as(ElseKey)) asNode Shape
    statementGrammar.addAlternative(ifThenElseGrammar)
  }

  def getElseStatements[T <: NodeLike](ifThen: T): Seq[T] = {
    ifThen(ElseKey).asInstanceOf[Seq[T]]
  }
}
