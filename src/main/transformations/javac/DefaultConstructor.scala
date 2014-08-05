package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.base.model.JavaClassModel
import transformations.javac.base.model.JavaMethodModel.PublicVisibility

object DefaultConstructor extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(ConstructorC)

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      JavaClassModel.getMethods(clazz).prepend(ConstructorC.constructor(Seq(), Seq(), PublicVisibility))
    }
  }
}
