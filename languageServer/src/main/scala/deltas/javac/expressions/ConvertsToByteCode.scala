package deltas.javac.expressions

import core.deltas.path.NodePath
import core.deltas.{Delta, HasShape, ShapeProperty}
import core.language.node.Node
import core.language.{Compilation, Language}

trait ConvertsToByteCode extends Delta with HasShape {
  def toByteCode(node: NodePath, compilation: Compilation): Seq[Node]

  override def inject(language: Language): Unit = {
    ToByteCodeSkeleton.toByteCode.add(language, shape, this)
    super.inject(language)
  }
}

object ToByteCodeSkeleton {
  val toByteCode = new ShapeProperty[ConvertsToByteCode]

  def getToInstructions(compilation: Compilation): NodePath => Seq[Node] = {
    expression => toByteCode.get(compilation, expression.shape).toByteCode(expression, compilation)
  }
}
