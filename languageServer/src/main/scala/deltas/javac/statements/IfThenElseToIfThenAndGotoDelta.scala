package deltas.javac.statements

import core.deltas.path.{NodeChildPath, NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithPhase}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.statement.IfThenElseDelta.IfThenElse
import deltas.statement._

object IfThenElseToIfThenAndGotoDelta extends DeltaWithPhase {

  override def description: String = "Compiles the if-then-else into if-then and goto."

  override def dependencies: Set[Contract] = Set(IfThenElseDelta, GotoStatementDelta, LabelStatementDelta, BlockDelta)

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(shape, path => transform(path, compilation))
  }

  def transform(ifElsePath: NodePath, language: Language): Unit = {
    val method = ifElsePath.findAncestorShape(MethodDelta.Shape)
    val ifThenElse: IfThenElse[NodePath] = ifElsePath
    val endLabel = LabelDelta.getUniqueLabel("ifThenElseEnd", method)
    val ifThen = IfThenDelta.neww(ifThenElse.condition, BlockDelta.neww(Seq(ifThenElse.thenStatement, GotoStatementDelta.neww(endLabel))))
    val replacement = BlockDelta.neww(Seq(ifThen,
      ifThenElse.elseStatement,
      LabelStatementDelta.neww(endLabel)))
    ifElsePath.asInstanceOf[NodeChildPath].replaceWith(replacement)
  }

  def shape: NodeShape = IfThenElseDelta.Shape
}
