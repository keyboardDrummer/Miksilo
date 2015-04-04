package transformations.javac.classes

import transformations.javac.classes.skeleton.{ClassInfo, PackageInfo}


trait ReferenceKind

case class PackageReference(info: PackageInfo) extends ReferenceKind

case class ClassOrObjectReference(info: ClassInfo, wasClass: Boolean) extends ReferenceKind