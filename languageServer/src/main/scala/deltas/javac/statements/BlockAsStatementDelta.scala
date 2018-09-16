package deltas.javac.statements

import core.deltas.Contract
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, SequenceElement}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.statement.{BlockDelta, StatementDelta, StatementInstance}

object BlockAsStatementDelta extends ByteCodeStatementInstance with StatementInstance {
  override def description: String = "Allows using a block as a statement, to create a new scope."

  override def dependencies: Set[Contract] = Set(BlockDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    val blockGrammar = find(BlockDelta.Grammar).as(Statements) asLabelledNode Shape
    statementGrammar.addAlternative(blockGrammar)
  }

  implicit class BlockStatement[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def statements: Seq[T] = node(Statements).asInstanceOf[Seq[T]]
  }

  object Shape extends NodeShape
  object Statements extends NodeField

  override def shape: NodeShape = Shape

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = { //TODO consider changing this to a transformation to statements, but then that would have to come after SolveConstraints
    val toInstructions = ByteCodeStatementSkeleton.getToInstructions(compilation)
    val block: BlockStatement[NodePath] = statement
    block.statements.flatMap(childStatement => toInstructions(childStatement))
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val block: BlockStatement[NodePath] = statement
    val blockScope = builder.newScope(Some(parentScope))
    block.statements.foreach(childStatement => ConstraintSkeleton.constraints(compilation, builder, childStatement, blockScope))
  }

  override def getLabels(statement: NodePath): Map[Any, NodePath] = {
    val block: BlockStatement[NodePath] = statement
    val childStatements = block.statements
    val nextMap: Map[Any, NodePath] = statement.asInstanceOf[SequenceElement].getNext.fold[Map[Any, NodePath]](
      Map.empty)(
      (next: NodePath) => Map(getNextLabel(childStatements.last) -> next))
    nextMap ++ super.getLabels(statement)
  }

  override def getNextStatements(statement: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = {
    val block: BlockStatement[NodePath] = statement
    val childStatements = block.statements
    Set(childStatements.head)
  }
}
