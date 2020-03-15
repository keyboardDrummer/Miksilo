package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.statement.LocalDeclarationDelta
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.classes.{ClassDelta, HasConstraintsDelta}

object StateVariableDeclarationDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Modifiers extends NodeField
  object Initializer extends NodeField // TODO replace this with FieldDeclarationWithInitializer

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val expression: BiGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val modifiers = (printSpace ~> ("public" | "internal" | "private" | "constant")).many.as(Modifiers)
    val initializer = (printSpace ~> "=" ~~> expression).option.as(Initializer)
    val grammar = typeGrammar.as(LocalDeclarationDelta.Type) ~ modifiers ~~ find(Name) ~ initializer ~ ";" asNode Shape
    find(ClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Introduce contract fields"

  override def dependencies = Set(TypeSkeleton, SolidityContractDelta, ExpressionDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    LocalDeclarationDelta.collectConstraints(compilation, builder, path, parentScope)
  }
}
