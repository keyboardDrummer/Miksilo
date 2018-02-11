package deltas.javaPlus

import core.language.node.Node
import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass
import deltas.javac.methods.AccessibilityFieldsDelta.HasAccessibility
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}

object ReorderMembers extends DeltaWithPhase {

  override def description: String = "Moves static before instance fields, and fields before members"

  override def dependencies: Set[Contract] = Set[Contract](JavaClassSkeleton, AccessibilityFieldsDelta, MethodDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val javaClass: JavaClass[Node] = program

    val methods = javaClass.members.filter(member => member.shape == MethodDelta.Shape)
    val fields = javaClass.members.filter(member => member.shape != MethodDelta.Shape)

    val orderedFields = fields.sortBy(f => !new HasAccessibility(f).isStatic)

    javaClass.members = orderedFields ++ methods
  }
}
