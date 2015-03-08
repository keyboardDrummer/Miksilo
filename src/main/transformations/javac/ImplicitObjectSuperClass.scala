package transformations.javac

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.{JavaClassSkeleton, QualifiedClassName}

object ImplicitObjectSuperClass extends ParticleWithPhase {
  val objectName = "Object"
  val packageName = Seq("java", "lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    if (JavaClassSkeleton.getParent(program).isEmpty) {
      program(JavaClassSkeleton.ClassParent) = Some(objectName)
    }
  }

  override def description: String = "Implicit adds Object as a class parent if no explicit parent is specified."
}
