package miksilo.modularLanguages.deltas.classes.constructor

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.javac.methods.AccessibilityFieldsDelta
import miksilo.modularLanguages.deltas.statement.BlockDelta

object DefaultConstructorDelta extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set.empty

  def transformProgram(program: Node, state: Compilation): Unit = {
    program.visitShape(ClassDelta.Shape, node => {
      val javaClass: JavaClass[Node] = node
      val noConstructors = !javaClass.members.exists(member => member.shape == ConstructorDelta.Shape)
      if (noConstructors) {
        val defaultConstructor = ConstructorDelta.constructor(javaClass.name, Seq(), BlockDelta.neww(), AccessibilityFieldsDelta.PublicVisibility)
        javaClass.members = Seq(defaultConstructor) ++ javaClass.members
      }
    })
  }

  override def description: String = "Adds a default public constructor to a class if none is specified."
}
