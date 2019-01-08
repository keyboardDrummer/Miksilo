package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.bytecode.types.TypeSkeleton

object MappingTypeDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val elementaryType = find(ElementaryTypeDelta.Shape)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val grammar = "mapping" ~~ (elementaryType ~~ "=>" ~~ typeGrammar).inParenthesis
    typeGrammar.addAlternative(grammar)
  }

  override def description = "Adds the mapping type"

  override def dependencies = Set(ElementaryTypeDelta, TypeSkeleton)
}
