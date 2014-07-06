package transformations.javac

import core.transformation.{TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.JavaBase
import transformations.javac.base.model.{JavaMethodModel, JavaClassModel}
import JavaMethodModel.PublicVisibility
import transformations.javac.base.model.JavaClassModel

object DefaultConstructor extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ConstructorC)

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      JavaClassModel.getMethods(clazz).prepend(ConstructorC.constructor(Seq(),Seq(), PublicVisibility))
    }
  }
}
