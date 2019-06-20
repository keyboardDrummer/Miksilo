package deltas.smithy

import core.deltas.Delta
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes.skeleton.HasConstraints
import deltas.{ConstraintSkeleton, FileWithMembersDelta}

object SmithyStandardLibrary extends Delta {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ConstraintSkeleton.hasConstraints.change(language, FileWithMembersDelta.Shape, original => {
      new HasConstraints(){
        override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                        path: NodePath, parentScope: Scope): Unit = {
          val traits = Seq("pattern","readonly","error","paginated","references")
          for(_trait <- traits) {
            builder.declare(_trait, parentScope, null, Some(TraitDelta.traitType))
          }
          original.collectConstraints(compilation, builder, path, parentScope)
        }
      }
    })
  }

  override def description = ""

  override def dependencies = Set(FileWithMembersDelta)
}
