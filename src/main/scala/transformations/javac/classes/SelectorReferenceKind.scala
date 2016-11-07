package transformations.javac.classes

import core.particles._
import core.particles.path.Path
import transformations.javac.classes.skeleton.{JavaClassSkeleton, ClassSignature, PackageSignature}
import transformations.javac.methods.MemberSelector
import transformations.javac.methods.MemberSelector.SelectorKey

object SelectorReferenceKind extends Delta {
  override def dependencies: Set[Contract] = Set(SelectField, JavaClassSkeleton)

  override def inject(state: CompilationState): Unit = {
    MemberSelector.getReferenceKindRegistry(state).put(SelectorKey, selector => {
      val compiler = JavaClassSkeleton.getClassCompiler(state)
      getReferenceKind(selector, compiler)
    })
  }

  def getReferenceKind(selector: Path, compiler: ClassCompiler): ReferenceKind = {
    val obj = MemberSelector.getSelectorObject(selector)
    val member = MemberSelector.getSelectorMember(selector)
    MemberSelector.getReferenceKind(compiler, obj) match {
      case PackageReference(info) => info.content(member) match {
        case result: PackageSignature => new PackageReference(result)
        case result: ClassSignature => new ClassOrObjectReference(result, true)
      }
      case ClassOrObjectReference(info, _) =>
        val field = info.getField(member)
        val fieldClassType = compiler.findClass(field._type)
        new ClassOrObjectReference(fieldClassType, false)
    }
  }

  override def description: String = "Enables recognizing the kind of a selection, whether is a class, package or object."
}
