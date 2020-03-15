package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, FileWithMembersDelta}

// TODO change this so that traits inject themselves
object ShapeStatementDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object ShapeBody extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val traits = find(TraitDelta.Traits)
    val shapeBody = create(ShapeBody).as(ShapeBody)
    val grammar = traits % shapeBody asLabelledNode Shape
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(grammar)
  }

  override def description = "Adds the shape statement concept"

  override def dependencies = Set(FileWithMembersDelta, TraitDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    ConstraintSkeleton.constraints(compilation, builder, path(ShapeBody).asInstanceOf[NodePath], parentScope)
    for(_trait <- path(TraitDelta.Traits).asInstanceOf[Seq[NodePath]]) {
      ConstraintSkeleton.constraints(compilation, builder, _trait, parentScope)
    }
  }
}
