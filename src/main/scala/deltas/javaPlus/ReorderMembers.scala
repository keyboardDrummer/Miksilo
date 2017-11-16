package deltas.javaPlus

import core.deltas.node.Node
import core.deltas.{Compilation, DeltaWithPhase}
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass
import deltas.javac.methods.AccessibilityFieldsDelta.HasAccessibility
import deltas.javac.methods.MethodDelta

object ReorderMembers extends DeltaWithPhase {
  override def transform(program: Node, state: Compilation): Unit = {
    val clazz: JavaClass[Node] = program

    val methods = clazz.members.filter(member => member.clazz == MethodDelta.Clazz)
    val fields = clazz.members.filter(member => member.clazz != MethodDelta.Clazz)

    val orderedFields = fields.sortBy(f => !new HasAccessibility(f).isStatic)

    clazz.members = orderedFields ++ methods
  }

  override def description: String = "Moves static before instance fields, and fields before members"
}
