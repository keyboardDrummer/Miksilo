package deltas.javac.constructor

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.methods.AccessibilityFieldsDelta

object DefaultConstructorDelta extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorDelta)

  def transformProgram(program: Node, state: Compilation): Unit = {
    val javaClass: JavaClass[Node] = program
    val noConstructors = !javaClass.members.exists(member => member.shape == ConstructorDelta.ConstructorKey)
    if (noConstructors) {
      val defaultConstructor = ConstructorDelta.constructor(javaClass.name, Seq(), Seq(), AccessibilityFieldsDelta.PublicVisibility)
      javaClass.members = Seq(defaultConstructor) ++ javaClass.members
    }
  }

  override def description: String = "Adds a default public constructor to a class if none is specified."
}
