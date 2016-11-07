package transformations.javac

import core.particles.node.Node
import core.particles.{CompilationState, Contract, DeltaWithPhase}
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}

object ImplicitObjectSuperClass extends DeltaWithPhase {
  val objectName = "Object"
  val packageName = Seq("java", "lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)

  override def transform(program: Node, state: CompilationState): Unit = {
    if (program.parent.isEmpty) {
      program.parent = Some(objectName)
    }
  }

  override def description: String = "Implicitly adds Object as a class parent if no explicit parent is specified."
}
