package deltas.javac.statements

import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import core.language.node.Node
import deltas.statement.ForLoopDelta.ForLoop
import deltas.statement._

import scala.collection.mutable

object ForLoopContinueDelta extends DeltaWithPhase {

  override def description: String = "Add proper C-style for-loop continue semantics."

  override def dependencies: Set[Contract] = Set(ForLoopDelta, WhileContinueDelta)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val beforeIncrementLabels = new mutable.HashMap[NodePath, String]()
    PathRoot(program).visitShape(WhileContinueDelta.ContinueKey).foreach(
      path => transformContinue(compilation, path, beforeIncrementLabels))
  }

  def transformContinue(compilation: Compilation, continuePath: NodePath, beforeIncrementLabels: mutable.Map[NodePath, String]): Unit = {
    val containingLoopOption = continuePath.ancestors.find(ancestor => ancestor.shape == ForLoopDelta.Shape || ancestor.shape == WhileLoopDelta.Shape)
    containingLoopOption.filter(ancestor => ancestor.shape == ForLoopDelta.Shape).foreach(containingForLoop => {
      val label = beforeIncrementLabels.getOrElseUpdate(containingForLoop, addAndReturnBeforeIncrementLabel(compilation, containingForLoop))
      continuePath.replaceData(GotoStatementDelta.neww(label))
    })
  }

  def addAndReturnBeforeIncrementLabel(compilation: Compilation, forLoopPath: NodePath): String = {
    val forLoop = forLoopPath.current
    val beforeIncrementLabel = LabelStatementDelta.getUniqueLabel(compilation, "beforeIncrement", forLoopPath)
    forLoop(ForLoopDelta.Body) = BlockDelta.neww(Seq(forLoop.body, LabelStatementDelta.neww(beforeIncrementLabel)))
    beforeIncrementLabel
  }
}
