package miksilo.modularLanguages.deltas.classes

import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape}
import miksilo.modularLanguages.deltas.HasNameDelta.HasName

object ClassDelta {

  override def toString: String = "ClassDelta"

  object Shape extends NodeShape {
    override def toString: String = "Class"
  }

  object Members extends NodeField {
    override def toString: String = "Members"
  }

  object ClassImports extends NodeField {
    override def toString: String = "Imports"
  }

  object ClassParent extends NodeField {
    override def toString: String = "Parent"
  }

  object ClassPackage extends NodeField {
    override def toString: String = "Package"
  }

  implicit class JavaClass[T <: NodeLike](val node: T) extends HasName[T] {
    def _package: Seq[String] = node(ClassPackage).asInstanceOf[Seq[String]]
    def _package_=(value: Seq[String]): Unit = node(ClassPackage) = value

    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]): Unit = node(ClassImports) = value

    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]): Unit = node(Members) = value

    def parent: Option[String] = node.getValue(ClassParent).asInstanceOf[Option[String]]
    def parent_=(value: Option[String]): Unit = node(ClassParent) = value
  }
}
