package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.base.MethodAndClassC
import transformations.javac.base.model.{JavaClassModel, QualifiedClassName}

object ImplicitObjectSuperClass extends ProgramTransformation {
  val objectName = "Object"
  val packageName = Seq("java", "lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))

  override def dependencies: Set[Contract] = Set(MethodAndClassC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    if (JavaClassModel.getParent(program).isEmpty) {
      program(JavaClassModel.ClassParent) = objectName
    }
  }
}
