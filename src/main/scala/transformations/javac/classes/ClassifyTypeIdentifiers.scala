package transformations.javac.classes

import core.particles.{CompilationState, DeltaWithPhase}
import core.particles.node.Node
import transformations.bytecode.types.ObjectTypeC
import transformations.javac.types.TypeVariable

object ClassifyTypeIdentifiers extends DeltaWithPhase {
  override def transform(program: Node, state: CompilationState): Unit = {
    program.visitOld(node => node.clazz match {
      case TypeVariable.TypeVariableKey =>
        val objectType = ObjectTypeC.objectType(TypeVariable.getTypeVariableName(node))
        node.replaceWith(objectType)
      case _ =>
    })
  }

  override def description: String = "Determines which TypeVariables are actually object types."
}
