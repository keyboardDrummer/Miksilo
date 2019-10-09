package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.NodePath
import core.document.BlankLine
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.PrimitiveType
import deltas.ConstraintSkeleton
import deltas.javac.classes.skeleton.HasConstraintsDelta

object FileWithMembersDelta extends DeltaWithGrammar with HasConstraintsDelta {
  object Shape extends NodeShape
  object Members extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member: BiGrammar = create(Members)
    find(BodyGrammar).inner = member.manySeparatedVertical(BlankLine).as(Members).asNode(Shape)
  }

  override def description = "Defines a file that contains different members of unknown members"

  override def dependencies = Set.empty

  override def shape = Shape

  val fileType = PrimitiveType("file")
  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val fileDeclaration = builder.declare(path.startOfUri.get, parentScope, _type = Some(fileType))
    val fileScope = builder.declareScope(fileDeclaration, Some(parentScope), s"file '${path.startOfUri.get}'")
    for(member <- path(Members).asInstanceOf[Seq[NodePath]]) {
      ConstraintSkeleton.constraints(compilation, builder, member, fileScope)
    }
  }
}






