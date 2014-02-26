package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.{JavaBase, JavaMethodModel, JavaClassModel}
import languages.javac.base.JavaMethodModel.PublicVisibility

object DefaultConstructor extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ConstructorC)

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      JavaClassModel.getMethods(clazz).prepend(ConstructorC.constructor(Seq(),Seq(), PublicVisibility))
    }
  }
}
