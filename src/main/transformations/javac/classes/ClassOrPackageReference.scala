package transformations.javac.classes

import core.transformation.sillyCodePieces.Injector
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.methods.VariableC
import transformations.javac.methods.VariableC.VariableKey

object ClassOrPackageReference extends Injector {
  override def inject(state: TransformationState): Unit = {
    ClassC.getReferenceKindRegistry(state).put(VariableKey, variable => {
      val compiler = ClassC.getClassCompiler(state)
      getReferenceKind(variable, compiler)
    })
  }

  def getReferenceKind(variable: MetaObject, classCompiler: ClassCompiler): ReferenceKind = {

    val name = VariableC.getVariableName(variable)
    val isClass = classCompiler.classNames.contains(name)
    if (isClass)
      new ClassOrObjectReference(classCompiler.findClass(name), true)
    else {
      val mbPackage = classCompiler.compiler.env.content.get(name)
      if (mbPackage.isDefined)
        new PackageReference(mbPackage.get.asInstanceOf[PackageInfo])
      else {
        classCompiler.getReferenceKindFromExpressionType(variable)
      }
    }
  }

  override def dependencies: Set[Contract] = Set(VariableC, ClassC)
}
