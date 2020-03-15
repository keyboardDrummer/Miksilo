package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.path.{NodeChildPath, NodePath, PathRoot}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.statement.IfThenElseDelta.IfThenElse
import miksilo.modularLanguages.deltas.statement._

object IfThenElseToIfThenAndGotoDelta extends DeltaWithPhase {

  override def description: String = "Compiles the if-then-else into if-then and goto."

  override def dependencies: Set[Contract] = Set(IfThenElseDelta, GotoStatementDelta, LabelStatementDelta, BlockDelta)

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(shape, path => transform(compilation, path))
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
