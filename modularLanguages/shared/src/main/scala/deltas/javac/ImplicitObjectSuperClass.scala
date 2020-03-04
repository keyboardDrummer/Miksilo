package deltas.javac

import core.language.node.Node
import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import deltas.classes.ClassDelta.JavaClass
import deltas.javac.classes.skeleton.{JavaClassDelta, QualifiedClassName}

object ImplicitObjectSuperClass extends DeltaWithPhase {
  val objectName = "Object"
  val packageName = Seq("java", "lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))

  override def dependencies: Set[Contract] = Set(JavaClassDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val clazz: JavaClass[Node] = program
    if (clazz.parent.isEmpty) {
      program.parent = Some(objectName)
    }
  }

  override def description: String = "Implicitly adds Object as a class parent if no explicit parent is specified."
}
