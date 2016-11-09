package transformations.javac.statements

import core.particles.{CompilationState, DeltaWithPhase}
import core.particles.node.Node
import core.particles.path.{Path, PathRoot}
import transformations.javac.statements.ForLoopC.ForLoop
import scala.collection.mutable

object ForLoopContinueC extends DeltaWithPhase {

  override def description: String = "Add proper C-style for-loop continue semantics."

  override def transform(program: Node, state: CompilationState): Unit = {
    val beforeIncrementLabels = new scala.collection.mutable.HashMap[Node, String]()
    PathRoot(program).visit(path => path.clazz match {
      case WhileContinueC.ContinueKey => transformContinue(path, beforeIncrementLabels, state)
      case _ =>
    })
  }

  def transformContinue(continuePath: Path, beforeIncrementLabels: mutable.Map[Node, String], state: CompilationState): Unit = {
    val containingLoopOption = continuePath.ancestors.find(ancestor => ancestor.clazz == ForLoopC.ForLoopType || ancestor.clazz == WhileC.WhileKey)
    containingLoopOption.filter(ancestor => ancestor.clazz == ForLoopC.ForLoopType).foreach(containingForLoop => {
      if (!beforeIncrementLabels.contains(containingForLoop))
      {
        beforeIncrementLabels(containingForLoop) = transformForLoop(containingForLoop, state)
      }
      val label = beforeIncrementLabels(containingForLoop)
      continuePath.replaceWith(JustJavaGoto.goto(label))
    })
  }

  def transformForLoop(forLoopPath: Path, state: CompilationState): String = {
    val forLoop = forLoopPath.current
    val beforeIncrementLabel = state.getUniqueLabel("beforeIncrement")
    forLoop(ForLoopC.Body) = forLoop.body ++ Seq(JustJavaLabel.label(beforeIncrementLabel))
    beforeIncrementLabel
  }
}
