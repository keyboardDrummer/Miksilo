package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.editorParser.document.BlankLine
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.PrimitiveType
import miksilo.modularLanguages.deltas.ConstraintSkeleton
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

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
    val fileScope = builder.declareScope(fileDeclaration, parentScope, s"file '${path.startOfUri.get}'")
    for(member <- path(Members).asInstanceOf[Seq[NodePath]]) {
      ConstraintSkeleton.constraints(compilation, builder, member, fileScope)
    }
  }
}






