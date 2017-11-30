package deltas.javac.statements

import core.deltas.node.Node
import core.deltas.path.{Path, PathRoot}
import core.deltas.{Compilation, Contract, DeltaWithPhase, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.javac.statements.ForLoopC.ForLoop

import scala.collection.mutable

object ForLoopContinueC extends DeltaWithPhase {

  override def description: String = "Add proper C-style for-loop continue semantics."

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val beforeIncrementLabels = new scala.collection.mutable.HashMap[Node, String]()
    PathRoot(program).visit(path => path.clazz match {
      case WhileContinueDelta.ContinueKey => transformContinue(path, beforeIncrementLabels, state)
      case _ =>
    })
  }

  def transformContinue(continuePath: Path, beforeIncrementLabels: mutable.Map[Node, String], state: Language): Unit = {
    val containingLoopOption = continuePath.ancestors.find(ancestor => ancestor.clazz == ForLoopC.ForLoopType || ancestor.clazz == WhileDelta.WhileKey)
    containingLoopOption.filter(ancestor => ancestor.clazz == ForLoopC.ForLoopType).foreach(containingForLoop => {
      val label = beforeIncrementLabels.getOrElseUpdate(containingForLoop, transformForLoop(containingForLoop, state))
      continuePath.replaceWith(JustJavaGoto.goto(label))
    })
  }

  def transformForLoop(forLoopPath: Path, state: Language): String = {
    val forLoop = forLoopPath.current
    val method = forLoopPath.findAncestorClass(MethodDelta.Clazz)
    val beforeIncrementLabel = LabelDelta.getUniqueLabel("beforeIncrement", method, state)
    forLoop(ForLoopC.Body) = forLoop.body ++ Seq(JustJavaLabel.label(beforeIncrementLabel))
    beforeIncrementLabel
  }

  override def dependencies: Set[Contract] = Set(ForLoopC, WhileContinueDelta)
}
