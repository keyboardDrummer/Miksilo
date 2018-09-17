package deltas.javac.statements

import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithPhase}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.statement.IfThenElseDelta.IfThenElse
import deltas.statement.{BlockDelta, IfThenDelta, IfThenElseDelta}

object IfThenElseToIfThenAndGotoDelta extends DeltaWithPhase {

  override def description: String = "Enables using the if-then-else construct."

  override def dependencies: Set[Contract] = super.dependencies ++
    Set(IfThenElseDelta, JavaGotoDelta, BlockDelta)

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(shape, path => transformBreak(path, compilation))
  }

  def transformBreak(ifElsePath: NodePath, language: Language): Unit = {
    val method = ifElsePath.findAncestorShape(MethodDelta.Shape)
    val ifThenElse: IfThenElse[NodePath] = ifElsePath
    val endLabel = LabelDelta.getUniqueLabel("ifThenElseEnd", method)
    BlockDelta.neww(Seq(IfThenDelta.neww(ifThenElse.condition, BlockDelta.neww(Seq(ifThenElse.thenStatement, JustJavaGoto.neww(endLabel)))),
      ifThenElse.elseStatement,
      JustJavaLabel.neww(endLabel)))
  }
  def shape: NodeShape = IfThenElseDelta.Shape
}
