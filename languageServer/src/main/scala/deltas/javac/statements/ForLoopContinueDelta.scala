package deltas.javac.statements

import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithPhase}
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.statement.ForLoopDelta.ForLoop
import deltas.statement._

import scala.collection.mutable

object ForLoopContinueDelta extends DeltaWithPhase {

  override def description: String = "Add proper C-style for-loop continue semantics."

  override def dependencies: Set[Contract] = Set(ForLoopDelta, WhileContinueDelta)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val beforeIncrementLabels = new mutable.HashMap[NodePath, String]()
    PathRoot(program).visitShape(WhileContinueDelta.ContinueKey).foreach(
      path => transformContinue(path, beforeIncrementLabels, compilation))
  }

  def transformContinue(continuePath: NodePath, beforeIncrementLabels: mutable.Map[NodePath, String], language: Language): Unit = {
    val containingLoopOption = continuePath.ancestors.find(ancestor => ancestor.shape == ForLoopDelta.Shape || ancestor.shape == WhileLoopDelta.Shape)
    containingLoopOption.filter(ancestor => ancestor.shape == ForLoopDelta.Shape).foreach(containingForLoop => {
      val label = beforeIncrementLabels.getOrElseUpdate(containingForLoop, addAndReturnBeforeIncrementLabel(containingForLoop))
      continuePath.replaceData(GotoStatementDelta.neww(label))
    })
  }

  def addAndReturnBeforeIncrementLabel(forLoopPath: NodePath): String = {
    val forLoop = forLoopPath.current
    val beforeIncrementLabel = LabelStatementDelta.getUniqueLabel("beforeIncrement", forLoopPath)
    forLoop(ForLoopDelta.Body) = BlockDelta.neww(Seq(forLoop.body, LabelStatementDelta.neww(beforeIncrementLabel)))
    beforeIncrementLabel
  }
}
