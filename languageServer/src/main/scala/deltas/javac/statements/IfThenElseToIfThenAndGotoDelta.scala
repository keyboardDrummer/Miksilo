package deltas.javac.statements

import core.deltas.path.{ChildPath, NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithPhase}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.statement.IfThenElseDelta.IfThenElse
import deltas.statement.{BlockDelta, IfThenDelta, IfThenElseDelta}

object IfThenElseToIfThenAndGotoDelta extends DeltaWithPhase {

  override def description: String = "Compiles the if-then-else into if-then and goto."

  override def dependencies: Set[Contract] = super.dependencies ++
    Set(IfThenElseDelta, JavaGotoDelta, BlockDelta)

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(shape, path => transform(path, compilation))
  }

  def transform(ifElsePath: NodePath, language: Language): Unit = {
    val method = ifElsePath.findAncestorShape(MethodDelta.Shape)
    val ifThenElse: IfThenElse[NodePath] = ifElsePath
    val endLabel = LabelDelta.getUniqueLabel("ifThenElseEnd", method)
    val ifThen = IfThenDelta.neww(ifThenElse.condition, BlockDelta.neww(Seq(ifThenElse.thenStatement, JustJavaGoto.neww(endLabel))))
    val replacement = BlockDelta.neww(Seq(ifThen,
      ifThenElse.elseStatement,
      JustJavaLabel.neww(endLabel)))
    ifElsePath.asInstanceOf[ChildPath].replaceWith(replacement)
  }

  def shape: NodeShape = IfThenElseDelta.Shape
}
