package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.types.TypeSkeleton
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.{HasConstraintsDelta, JavaClassDelta}
import deltas.statement.LocalDeclarationDelta
import deltas.HasNameDelta.Name

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
    find(JavaClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Introduce contract fields"

  override def dependencies = Set(TypeSkeleton, SolidityContractDelta, ExpressionDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    LocalDeclarationDelta.collectConstraints(compilation, builder, path, parentScope)
  }
}
