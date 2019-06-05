package deltas

import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar}
import core.document.BlankLine
import core.language.Language
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}

object FileWithMembersDelta extends DeltaWithGrammar {
  override def description: String = "Defines a file with members"

  object Shape extends NodeShape
  object Members extends NodeField

  implicit class FileWithMembers[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def members: Seq[T] = node(Members).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member = create(Members)
    val file = member.someSeparatedVertical(BlankLine).as(Members).asNode(Shape)
    find(BodyGrammar).inner = file
  }

  override def dependencies: Set[Contract] = Set.empty

  override def inject(language: Language): Unit = {
    super.inject(language)

    language.collectConstraints = (compilation, builder) => {
      val fileScope = builder.newScope(None, "fileScope")
      val members: Seq[NodePath] = PathRoot(compilation.program)(Members).asInstanceOf[Seq[NodePath]]
      for(member <- members) {
        ConstraintSkeleton.constraints(compilation, builder, member, fileScope)
      }
    }
  }
}
