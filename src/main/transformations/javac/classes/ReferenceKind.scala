package transformations.javac.classes


trait ReferenceKind

case class PackageReference(info: PackageInfo) extends ReferenceKind

case class ClassOrObjectReference(info: ClassInfo, wasClass: Boolean) extends ReferenceKind