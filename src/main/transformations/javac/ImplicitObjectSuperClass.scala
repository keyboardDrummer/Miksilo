package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.{ClassC, QualifiedClassName}

object ImplicitObjectSuperClass extends ProgramTransformation {
  val objectName = "Object"
  val packageName = Seq("java", "lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))

  override def dependencies: Set[Contract] = Set(ClassC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    if (ClassC.getParent(program).isEmpty) {
      program(ClassC.ClassParent) = objectName
    }
  }
}
