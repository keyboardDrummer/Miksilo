package deltas.solidity

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.RegexGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.HasConstraintsDelta

object PragmaDelta extends DeltaWithGrammar with HasConstraintsDelta {
  object Shape extends NodeShape
  object Name extends NodeField
  object Values extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar: BiGrammar = "pragma" ~~ identifier.as(Name) ~~ (RegexGrammar("""[^;]+""".r, "pragma value") | expression).as(Values) ~ ";" asNode Shape
    find(FileWithMembersDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds pragmas"

  override def dependencies = Set(FileWithMembersDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {

  }
}
