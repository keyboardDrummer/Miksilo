package deltas.statement

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.statement.IfThenDelta._

object IfThenElseDelta extends DeltaWithGrammar with StatementInstance {

  override def description: String = "Enables using the if-then-else construct."

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta, BlockDelta)

  def neww(condition: Node, thenBody: Any, elseBody: Any): Node =
    Shape.create(Condition -> condition, Then -> thenBody, ElseBody -> elseBody)

  object Shape extends NodeShape
  object ElseBody extends NodeField

  implicit class IfThenElse[T <: NodeLike](node: T) extends IfThen[T](node) {
    def elseStatement: T = node(ElseBody).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = find(IfThenDelta.Shape)
    val ifThenElseGrammar = ifThenGrammar.inner.asInstanceOf[NodeGrammar].inner ~ ("else" ~> bodyGrammar.as(ElseBody)) asNode Shape
    statementGrammar.addAlternative(ifThenElseGrammar)
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    IfThenDelta.collectConstraints(compilation, builder, statement, parentScope)
    val ifThenElse: IfThenElse[NodePath] = statement
    ConstraintSkeleton.constraints(compilation, builder, ifThenElse.elseStatement, parentScope)
  }

  override def shape: NodeShape = Shape
}
