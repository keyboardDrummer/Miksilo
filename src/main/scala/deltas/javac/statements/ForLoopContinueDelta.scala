package deltas.javac.statements

import core.deltas.node.Node
import core.deltas.path.{Path, PathRoot}
import core.deltas.{Compilation, Contract, DeltaWithPhase, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.javac.statements.ForLoopDelta.ForLoop

import scala.collection.mutable

object ForLoopContinueDelta extends DeltaWithPhase {

  override def description: String = "Add proper C-style for-loop continue semantics."

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val beforeIncrementLabels = new mutable.HashMap[Path, String]()
    PathRoot(program).visitClass(WhileContinueDelta.ContinueKey).foreach(
      path => transformContinue(path, beforeIncrementLabels, compilation))
  }

  def transformContinue(continuePath: Path, beforeIncrementLabels: mutable.Map[Path, String], language: Language): Unit = {
    val containingLoopOption = continuePath.ancestors.find(ancestor => ancestor.clazz == ForLoopDelta.Clazz || ancestor.clazz == WhileDelta.WhileKey)
    containingLoopOption.filter(ancestor => ancestor.clazz == ForLoopDelta.Clazz).foreach(containingForLoop => {
      val label = beforeIncrementLabels.getOrElseUpdate(containingForLoop, addAndReturnBeforeIncrementLabel(containingForLoop))
      continuePath.replaceWith(JustJavaGoto.goto(label))
    })
  }

  def addAndReturnBeforeIncrementLabel(forLoopPath: Path): String = {
    val forLoop = forLoopPath.current
    val method = forLoopPath.findAncestorClass(MethodDelta.Clazz)
    val beforeIncrementLabel = LabelDelta.getUniqueLabel("beforeIncrement", method)
    forLoop(ForLoopDelta.Body) = forLoop.body ++ Seq(JustJavaLabel.label(beforeIncrementLabel))
    beforeIncrementLabel
  }

  override def dependencies: Set[Contract] = Set(ForLoopDelta, WhileContinueDelta)
}
