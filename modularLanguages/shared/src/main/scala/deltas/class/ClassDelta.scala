package deltas.classes

import core.language.node.{NodeField, NodeLike, NodeShape}
import deltas.HasNameDelta.HasName

object ClassDelta {

  object Shape extends NodeShape

  object Members extends NodeField

  object ClassImports extends NodeField

  object ClassParent extends NodeField

  object ClassPackage extends NodeField

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
