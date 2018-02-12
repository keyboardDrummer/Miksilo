package deltas.javac.classes

import core.deltas._
import core.deltas.path.NodePath
import core.language.Language
import deltas.javac.classes.skeleton.{JavaClassSkeleton, PackageSignature}
import deltas.javac.methods.{MemberSelector, VariableDelta}
import deltas.javac.methods.VariableDelta.Shape

object VariableReferenceKind extends Delta {
  override def inject(state: Language): Unit = {
    MemberSelector.getReferenceKindRegistry(state).put(Shape, (compilation, variable) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      getReferenceKind(variable, compiler)
    })
  }

  def getReferenceKind(variable: NodePath, classCompiler: ClassCompiler): ReferenceKind = {

    val name = VariableDelta.getVariableName(variable)
    val isClass = classCompiler.classNames.contains(name)
    if (isClass)
      new ClassOrObjectReference(classCompiler.findClass(name), true)
    else {
      val mbPackage = classCompiler.javaCompiler.classPath.content.get(name)
      if (mbPackage.isDefined)
        new PackageReference(mbPackage.get.asInstanceOf[PackageSignature])
      else {
        MemberSelector.getReferenceKindFromExpressionType(classCompiler, variable)
      }
    }
  }

  override def dependencies: Set[Contract] = Set(VariableDelta, JavaClassSkeleton)

  override def description: String = "Enables recognizing the kind of an identifier, whether is a class, package or object."
}
