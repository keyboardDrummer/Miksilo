package deltas

import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.{NodePath, PathRoot}
import core.deltas.{Contract, DeltaWithGrammar}
import core.document.BlankLine
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.HasConstraintsDelta

object FileWithMembersDelta extends DeltaWithGrammar with HasConstraintsDelta {
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
      ConstraintSkeleton.constraints(compilation, builder, PathRoot(compilation.program), fileScope)
    }
  }

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val members: Seq[NodePath] = path(Members).asInstanceOf[Seq[NodePath]]
    for(member <- members) {
      ConstraintSkeleton.constraints(compilation, builder, member, parentScope)
    }
  }
}
