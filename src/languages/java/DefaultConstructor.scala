package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.java.base.{JavaBase, JavaMethodModel, JavaClassModel}
import languages.java.base.JavaMethodModel.PublicVisibility

object DefaultConstructor extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ConstructorC)

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      JavaClassModel.getMethods(clazz).append(ConstructorC.constructor(Seq(),Seq(), PublicVisibility))
    }
  }
}
