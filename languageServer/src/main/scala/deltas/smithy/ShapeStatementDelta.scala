package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.{ConstraintSkeleton, FileWithMembersDelta}
import deltas.javac.classes.skeleton.HasConstraintsDelta

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
