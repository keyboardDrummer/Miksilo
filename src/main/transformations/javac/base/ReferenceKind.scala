package transformations.javac.base


trait ReferenceKind

case class PackageReference(info: PackageInfo) extends ReferenceKind

case class ClassOrObjectReference(info: ClassInfo, wasClass: Boolean) extends ReferenceKind