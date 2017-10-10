package transformations.javac.classes

import core.particles._
import core.particles.path.Path
import transformations.javac.classes.skeleton.{JavaClassSkeleton, ClassSignature, PackageSignature}
import transformations.javac.methods.MemberSelector
import transformations.javac.methods.MemberSelector.Clazz

object SelectorReferenceKind extends Delta {
  override def dependencies: Set[Contract] = Set(SelectField, JavaClassSkeleton)

  override def inject(state: Language): Unit = {
    MemberSelector.getReferenceKindRegistry(state).put(Clazz, (compilation, selector) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      getReferenceKind(selector, compiler)
    })
  }

  def getReferenceKind(selector: Path, compiler: ClassCompiler): ReferenceKind = {
    val obj = MemberSelector.getSelectorObject(selector)
    val member = MemberSelector.getSelectorMember(selector)
    MemberSelector.getReferenceKind(compiler, obj) match {
      case PackageReference(info) => info.content(member) match {
        case result: PackageSignature => PackageReference(result)
        case result: ClassSignature => ClassOrObjectReference(result, wasClass = true)
      }
      case ClassOrObjectReference(info, _) =>
        val field = info.getField(member)
        val fieldClassType = compiler.findClass(field._type)
        ClassOrObjectReference(fieldClassType, wasClass = false)
    }
  }

  override def description: String = "Enables recognizing the kind of a selection, whether is a class, package or object."
}
