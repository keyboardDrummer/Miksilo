package transformations.javac

import core.transformation.{MetaObject, ProgramTransformation, TransformationState}
import transformations.javac.base.model.JavaClassModel
import transformations.javac.base.model.JavaMethodModel.PublicVisibility

object DefaultConstructor extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ConstructorC)

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      JavaClassModel.getMethods(clazz).prepend(ConstructorC.constructor(Seq(), Seq(), PublicVisibility))
    }
  }
}
