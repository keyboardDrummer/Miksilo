package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.bytecode.types.TypeSkeleton
import deltas.expression.ExpressionDelta

object StateVariableDeclarationDelta extends DeltaWithGrammar {
  //TypeName ( 'public' | 'internal' | 'private' | 'constant' )* Identifier ('=' Expression)? ';'

  object Shape extends NodeShape
  object Type extends NodeField
  object Name extends NodeField
  object Modifiers extends NodeField
  object Initializer extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val expression: BiGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val modifiers = (printSpace ~ ("public" | "internal" | "private" | "constant")).many.as(Modifiers)
    val initializer = (printSpace ~ "=" ~~ expression).option.as(Initializer)
    val grammar = typeGrammar.as(Type) ~ modifiers ~~ identifier.as(Name) ~ initializer ~ ";" asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Introduce contract fields"

  override def dependencies = Set(TypeSkeleton, SolidityContractDelta, ExpressionDelta)
}
