package transformations.javac.classes

import core.transformation.sillyCodePieces.Particle
import core.transformation.{Contract, MetaObject, TransformationState}
import SelectorC.SelectorKey

object ClassOrPackageSelector extends Particle {
  override def dependencies: Set[Contract] = Set(SelectorC, ClassC)

  override def inject(state: TransformationState): Unit = {
    ClassC.getReferenceKindRegistry(state).put(SelectorKey, selector => {
      val compiler = ClassC.getClassCompiler(state)
      getReferenceKind(selector, compiler)
    })
  }

  def getReferenceKind(selector: MetaObject, compiler: ClassCompiler): ReferenceKind = {
    val obj = SelectorC.getSelectorObject(selector)
    val member = SelectorC.getSelectorMember(selector)
    compiler.getReferenceKind(obj) match {
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
}
