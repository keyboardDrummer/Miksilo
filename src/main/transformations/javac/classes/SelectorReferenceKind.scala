package transformations.javac.classes

import core.particles.{CompilationState, Contract, MetaObject, Particle}
import transformations.javac.classes.SelectorC.SelectorKey

object SelectorReferenceKind extends Particle {
  override def dependencies: Set[Contract] = Set(SelectorC, JavaClassSkeleton)

  override def inject(state: CompilationState): Unit = {
    SelectorC.getReferenceKindRegistry(state).put(SelectorKey, selector => {
      val compiler = JavaClassSkeleton.getClassCompiler(state)
      getReferenceKind(selector, compiler)
    })
  }

  def getReferenceKind(selector: MetaObject, compiler: ClassCompiler): ReferenceKind = {
    val obj = SelectorC.getSelectorObject(selector)
    val member = SelectorC.getSelectorMember(selector)
    SelectorC.getReferenceKind(compiler, obj) match {
      case PackageReference(info) => info.content(member) match {
        case result: PackageInfo => new PackageReference(result)
        case result: ClassInfo => new ClassOrObjectReference(result, true)
      }
      case ClassOrObjectReference(info, _) =>
        val field = info.getField(member)
        val fieldClassType = compiler.findClass(field._type)
        new ClassOrObjectReference(fieldClassType, false)
    }
  }

  override def description: String = "Enables recognizing the kind of a selection, whether is a class, package or object."
}
