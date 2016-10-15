package transformations.javac.statements

import core.particles.{CompilationState, ParticleWithPhase}
import core.particles.node.Node
import core.particles.path.{Path, PathRoot}
import transformations.javac.statements.ForLoopC.ForLoop
import scala.collection.mutable

object ForLoopContinueC extends ParticleWithPhase {

  override def description: String = "Add proper C-style for-loop continue semantics."

  override def transform(program: Node, state: CompilationState): Unit = {
    val forLoops = new scala.collection.mutable.HashMap[Node, String]()
    PathRoot(program).foreach(path => path.clazz match {
      case WhileContinueC.ContinueKey => transformContinue(path, forLoops, state)
      case _ =>
    })
  }

  def transformContinue(continuePath: Path, beforeIncrementLabels: mutable.Map[Node, String], state: CompilationState): Unit = {
    val parent = continuePath.ancestors.filter(ancestor => ancestor.clazz == ForLoopC.ForLoopKey).head
    if (!beforeIncrementLabels.contains(parent))
    {
      beforeIncrementLabels(parent) = transformForLoop(parent, state)
    }
    val label = beforeIncrementLabels(parent)
    continuePath.replaceWith(JustJavaGoto.goto(label))
  }

  def transformForLoop(forLoopPath: Path, state: CompilationState): String = {
    val forLoop = forLoopPath.current
    val beforeIncrementLabel = state.getUniqueLabel("beforeIncrement")
    forLoop(ForLoopC.BodyKey) = forLoop.body ++ Seq(JustJavaLabel.label(beforeIncrementLabel))
    beforeIncrementLabel
  }
}
