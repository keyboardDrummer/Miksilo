package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.deltas.javac.classes.skeleton.{ClassSignature, PackageSignature}


trait ReferenceKind

case class PackageReference(info: PackageSignature) extends ReferenceKind

case class ClassOrObjectReference(info: ClassSignature, wasClass: Boolean) extends ReferenceKind