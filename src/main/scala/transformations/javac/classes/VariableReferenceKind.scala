package transformations.javac.classes

import core.particles._
import core.particles.path.Path
import transformations.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature}
import transformations.javac.methods.{MemberSelector, VariableC}
import transformations.javac.methods.VariableC.VariableKey

object VariableReferenceKind extends Delta {
  override def inject(state: Language): Unit = {
    MemberSelector.getReferenceKindRegistry(state).put(VariableKey, (compilation, variable) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      getReferenceKind(variable, compiler)
    })
  }

  def getReferenceKind(variable: Path, classCompiler: ClassCompiler): ReferenceKind = {

    val name = VariableC.getVariableName(variable)
    val isClass = classCompiler.classNames.contains(name)
    if (isClass)
      new ClassOrObjectReference(classCompiler.findClass(name), true)
    else {
      val mbPackage = classCompiler.javaCompilerState.classPath.content.get(name)
      if (mbPackage.isDefined)
        new PackageReference(mbPackage.get.asInstanceOf[PackageSignature])
      else {
        MemberSelector.getReferenceKindFromExpressionType(classCompiler, variable)
      }
    }
  }

  override def dependencies: Set[Contract] = Set(VariableC, JavaClassSkeleton)

  override def description: String = "Enables recognizing the kind of an identifier, whether is a class, package or object."
}
