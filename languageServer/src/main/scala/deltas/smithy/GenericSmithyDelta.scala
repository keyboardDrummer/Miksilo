package deltas.smithy

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Delta, DeltaWithGrammar}
import core.language.node.GrammarKey
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.HasConstraints
import deltas.json.JsonObjectLiteralDelta.MemberKey
import deltas.json.{JsonObjectLiteralDelta, SingleQuotedStringLiteralDelta, StringLiteralDelta}
import deltas.{ConstraintSkeleton, FileWithMembersDelta}

object GenericSmithyDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    find(JsonObjectLiteralDelta.MemberKey).addAlternative(identifier.as(MemberKey))

    val plainString = identifier.as(StringLiteralDelta.Value) asNode StringLiteralDelta.Shape
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(plainString)

    create(TextGrammar, plainString | find(StringLiteralDelta.Shape) | find(SingleQuotedStringLiteralDelta.Grammar))
  }

  object TextGrammar extends GrammarKey

  override def description = ""

  override def dependencies = Set(SingleQuotedStringLiteralDelta, StringLiteralDelta)
}

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


