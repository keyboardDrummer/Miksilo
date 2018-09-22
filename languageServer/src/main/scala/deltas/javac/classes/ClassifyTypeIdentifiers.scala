package deltas.javac.classes

import core.deltas.DeltaWithPhase
import core.language.node.Node
import core.language.Compilation
import deltas.bytecode.types.UnqualifiedObjectTypeDelta
import deltas.javac.types.TypeVariableDelta

object ClassifyTypeIdentifiers extends DeltaWithPhase {
  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    program.visit(node => node.shape match {
      case TypeVariableDelta.Shape =>
        val objectType = UnqualifiedObjectTypeDelta.neww(TypeVariableDelta.getTypeVariableName(node))
        node.replaceData(objectType)
      case _ =>
    })
  }

  override def description: String = "Determines which TypeVariables are actually object types."
}
