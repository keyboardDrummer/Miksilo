package deltas.javac.classes

import core.deltas.{Compilation, DeltaWithPhase}
import core.deltas.node.Node
import deltas.bytecode.types.UnqualifiedObjectTypeDelta
import deltas.javac.types.TypeVariable

object ClassifyTypeIdentifiers extends DeltaWithPhase {
  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    program.visit(node => node.shape match {
      case TypeVariable.Shape =>
        val objectType = UnqualifiedObjectTypeDelta.neww(TypeVariable.getTypeVariableName(node))
        node.replaceWith(objectType)
      case _ =>
    })
  }

  override def description: String = "Determines which TypeVariables are actually object types."
}
