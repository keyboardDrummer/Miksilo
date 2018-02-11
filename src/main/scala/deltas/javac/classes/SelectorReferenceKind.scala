package deltas.javac.classes

import core.deltas._
import core.deltas.path.NodePath
import core.language.Language
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton, PackageSignature}
import deltas.javac.methods.MemberSelector
import deltas.javac.methods.MemberSelector.Shape

object SelectorReferenceKind extends Delta {
  override def dependencies: Set[Contract] = Set(SelectField, JavaClassSkeleton)

  override def inject(state: Language): Unit = {
    MemberSelector.getReferenceKindRegistry(state).put(Shape, (compilation, selector) => {
      val compiler = JavaClassSkeleton.getClassCompiler(compilation)
      getReferenceKind(selector, compiler)
    })
  }

  def getReferenceKind(selector: NodePath, compiler: ClassCompiler): ReferenceKind = {
    val obj = MemberSelector.getSelectorTarget(selector)
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
