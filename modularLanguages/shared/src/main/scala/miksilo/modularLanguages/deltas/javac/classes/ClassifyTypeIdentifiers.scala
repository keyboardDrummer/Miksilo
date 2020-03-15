package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.deltas.bytecode.types.UnqualifiedObjectTypeDelta
import miksilo.modularLanguages.deltas.javac.types.TypeVariableDelta

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

  override def dependencies: Set[Contract] = Set(UnqualifiedObjectTypeDelta, TypeVariableDelta)
}
