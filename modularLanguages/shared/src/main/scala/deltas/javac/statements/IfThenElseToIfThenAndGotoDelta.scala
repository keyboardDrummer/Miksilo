package deltas.javac.statements

import core.deltas.path.{NodeChildPath, NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import core.language.node._
import deltas.statement.IfThenElseDelta.IfThenElse
import deltas.statement._

object IfThenElseToIfThenAndGotoDelta extends DeltaWithPhase {

  override def description: String = "Compiles the if-then-else into if-then and goto."

  override def dependencies: Set[Contract] = Set(IfThenElseDelta, GotoStatementDelta, LabelStatementDelta, BlockDelta)

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot.fromCompilation(compilation).visitShape(shape, path => transform(compilation, path))
  }

  def transform(compilation: Compilation, ifElsePath: NodePath): Unit = {
    val ifThenElse: IfThenElse[NodePath] = ifElsePath
    val endLabel = LabelStatementDelta.getUniqueLabel(compilation, "ifThenElseEnd", ifElsePath)
    val ifThen = IfThenDelta.neww(ifThenElse.condition, BlockDelta.neww(Seq(ifThenElse.thenStatement, GotoStatementDelta.neww(endLabel))))
    val replacement = BlockDelta.neww(Seq(ifThen,
      ifThenElse.elseStatement,
      LabelStatementDelta.neww(endLabel)))
    ifElsePath.asInstanceOf[NodeChildPath].replaceWith(replacement)
  }

  def shape: NodeShape = IfThenElseDelta.Shape
}
