package transformations.javac.constructor

import core.particles.node.Node
import core.particles.{Compilation, Contract, DeltaWithPhase, Language}
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.methods.MethodDelta.PublicVisibility

object DefaultConstructorC extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorC)

  def transform(clazz: Node, state: Compilation): Unit = {
    val noConstructors = clazz.members.filter(member => member.clazz == ConstructorC.ConstructorKey).isEmpty
    if (noConstructors) {
      val defaultConstructor = ConstructorC.constructor(clazz.name, Seq(), Seq(), PublicVisibility)
      clazz.members = Seq(defaultConstructor) ++ clazz.members
    }
  }

  override def description: String = "Adds a default public constructor to a class if none is specified."
}
