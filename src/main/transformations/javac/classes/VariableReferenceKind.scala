package transformations.javac.classes

import core.particles.{CompilationState, Contract, MetaObject, Particle}
import transformations.javac.methods.{MemberSelector, VariableC}
import transformations.javac.methods.VariableC.VariableKey

object VariableReferenceKind extends Particle {
  override def inject(state: CompilationState): Unit = {
    MemberSelector.getReferenceKindRegistry(state).put(VariableKey, variable => {
      val compiler = JavaClassSkeleton.getClassCompiler(state)
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
        MemberSelector.getReferenceKindFromExpressionType(classCompiler, variable)
      }
    }
  }

  override def dependencies: Set[Contract] = Set(VariableC, JavaClassSkeleton)

  override def description: String = "Enables recognizing the kind of an identifier, whether is a class, package or object."
}
