package deltas.statement

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import IfThenDelta._
object IfThenElseDelta extends DeltaWithGrammar with StatementInstance {

  override def description: String = "Enables using the if-then-else construct."

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta, BlockDelta)

  def neww(condition: Node, thenBody: Any, elseBody: Any): Node =
    Shape.create(Condition -> condition, Then -> thenBody, ElseBody -> elseBody)

  object Shape extends NodeShape
  object ElseBody extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = find(IfThenDelta.Shape)
    val ifThenElseGrammar = ifThenGrammar.inner.asInstanceOf[NodeGrammar].inner ~ ("else" ~> bodyGrammar.as(ElseBody)) asNode Shape
    statementGrammar.addAlternative(ifThenElseGrammar)
  }

  def getElseStatements[T <: NodeLike](ifThen: T): Seq[T] = {
    ifThen(ElseBody).asInstanceOf[Seq[T]]
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    IfThenDelta.collectConstraints(compilation, builder, statement, parentScope)
    val elseBodyScope = builder.newScope(Some(parentScope), "elseScope")
    val elseBody = getElseStatements(statement)
    BlockDelta.collectConstraints(compilation, builder, elseBody, elseBodyScope)
  }

  override def shape: NodeShape = Shape
}
