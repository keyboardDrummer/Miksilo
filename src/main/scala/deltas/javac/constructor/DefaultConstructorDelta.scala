package deltas.javac.constructor

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.methods.AccessibilityFieldsDelta

object DefaultConstructorDelta extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorDelta)

  def transform(clazz: Node, state: Compilation): Unit = {
    val noConstructors = !clazz.members.exists(member => member.clazz == ConstructorDelta.ConstructorKey)
    if (noConstructors) {
      val defaultConstructor = ConstructorDelta.constructor(clazz.name, Seq(), Seq(), AccessibilityFieldsDelta.PublicVisibility)
      clazz.members = Seq(defaultConstructor) ++ clazz.members
    }
  }

  override def description: String = "Adds a default public constructor to a class if none is specified."
}
