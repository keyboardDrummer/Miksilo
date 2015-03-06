package transformations.javac.constructor

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.ClassC
import transformations.javac.methods.MethodC.PublicVisibility

object DefaultConstructorC extends ParticleWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorC)

  def transform(clazz: MetaObject, state: TransformationState): Unit = {
    val className = ClassC.getClassName(clazz)
    val members = ClassC.getMembers(clazz)
    val constructor = ConstructorC.constructor(className, Seq(), Seq(), PublicVisibility)
    clazz(ClassC.Members) = Seq(constructor) ++ members
  }
}
