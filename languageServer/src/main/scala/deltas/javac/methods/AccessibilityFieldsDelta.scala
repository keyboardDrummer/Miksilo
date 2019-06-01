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
    create(Static, ("static" ~~> valueGrammar(true) | valueGrammar(false)) as Static)

    create(VisibilityField,
      "public" ~~> valueGrammar(PublicVisibility) |
        "protected" ~~> valueGrammar(ProtectedVisibility) |
        "private" ~~> valueGrammar(PrivateVisibility) |
        valueGrammar(DefaultVisibility) as VisibilityField)
  }

  override def description: String = "Define static and visibility"

  trait HasAccessibility[T <: NodeLike] extends NodeWrapper[T] {

    def visibility: Visibility = node.getValue(AccessibilityFieldsDelta.VisibilityField).asInstanceOf[Visibility]

    def isStatic: Boolean = node.getValue(AccessibilityFieldsDelta.Static).asInstanceOf[Boolean]
  }

  def addAccessFlags[T <: NodeLike](method: HasAccessibility[T]): Unit = {
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
