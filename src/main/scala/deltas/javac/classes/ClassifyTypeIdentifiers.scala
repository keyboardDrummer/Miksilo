package deltas.javac.classes

import core.particles.{Compilation, DeltaWithPhase, Language}
import core.particles.node.Node
import deltas.bytecode.types.ObjectTypeDelta
import deltas.javac.types.TypeVariable

object ClassifyTypeIdentifiers extends DeltaWithPhase {
  override def transform(program: Node, state: Compilation): Unit = {
    program.visit(node => node.clazz match {
      case TypeVariable.TypeVariableKey =>
        val objectType = ObjectTypeDelta.objectType(TypeVariable.getTypeVariableName(node))
        node.replaceWith(objectType)
      case _ =>
    })
  }

  override def description: String = "Determines which TypeVariables are actually object types."
}
