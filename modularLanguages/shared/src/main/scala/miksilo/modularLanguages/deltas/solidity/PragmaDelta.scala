package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object PragmaDelta extends DeltaWithGrammar with HasConstraintsDelta {
  object Shape extends NodeShape
  object Name extends NodeField
  object Values extends NodeField

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar: BiGrammar = "pragma" ~~ identifier.as(Name) ~~
      (grammars.regexGrammar("""[^;]+""".r, "pragma value") | expression).as(Values) ~ ";" asNode Shape
    find(FileWithMembersDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds pragmas"

  override def dependencies = Set(FileWithMembersDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {

  }
}
