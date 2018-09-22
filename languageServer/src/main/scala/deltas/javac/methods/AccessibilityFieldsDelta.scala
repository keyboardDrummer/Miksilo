package deltas.javac.methods

import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.bytecode.ByteCodeMethodInfo

object AccessibilityFieldsDelta extends DeltaWithGrammar {

  trait Visibility extends NodeShape

  object PublicVisibility extends Visibility

  object ProtectedVisibility extends Visibility

  object PrivateVisibility extends Visibility

  object DefaultVisibility extends Visibility

  object Static extends NodeField

  object VisibilityField extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    create(Static, "static" ~~> value(true) | value(false) as Static)

    create(VisibilityField,
      "public" ~~> value(PublicVisibility) |
        "protected" ~~> value(ProtectedVisibility) |
        "private" ~~> value(PrivateVisibility) |
        value(DefaultVisibility) as VisibilityField)
  }

  override def description: String = "Define static and visibility"

  implicit class HasAccessibility[T <: NodeLike](val node: T) extends NodeWrapper[T] {

    def visibility: Visibility = node(AccessibilityFieldsDelta.VisibilityField).asInstanceOf[Visibility]

    def isStatic: Boolean = node(AccessibilityFieldsDelta.Static).asInstanceOf[Boolean]
  }

  def addAccessFlags[T <: NodeLike](method: HasAccessibility[T]) = {
    var flags = Set[ByteCodeMethodInfo.MethodAccessFlag]()
    if (method.isStatic)
      flags += ByteCodeMethodInfo.StaticAccess

    flags ++= visibilityToAccessFlag(method.visibility)

    method(ByteCodeMethodInfo.AccessFlagsKey) = flags
  }

  val visibilityToAccessFlag = visibilityAccessFlagLinks.toMap
  def visibilityAccessFlagLinks: Seq[(Visibility, Set[ByteCodeMethodInfo.MethodAccessFlag])] = Seq(
    (PublicVisibility, Set[ByteCodeMethodInfo.MethodAccessFlag](ByteCodeMethodInfo.PublicAccess)),
    (PrivateVisibility, Set[ByteCodeMethodInfo.MethodAccessFlag](ByteCodeMethodInfo.PrivateAccess)),
    (DefaultVisibility, Set.empty[ByteCodeMethodInfo.MethodAccessFlag])
  )

  override def dependencies: Set[Contract] = Set.empty
}
