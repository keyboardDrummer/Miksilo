package transformations.javac.classes

import transformations.javac.classes.skeleton.{ClassSignature, PackageSignature}


trait ReferenceKind

case class PackageReference(info: PackageSignature) extends ReferenceKind

case class ClassOrObjectReference(info: ClassSignature, wasClass: Boolean) extends ReferenceKind