package miksilo.modularLanguages.deltas.bytecode.types

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar, HasShape}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton.HasSuperTypes

trait TypeInstance extends DeltaWithGrammar with HasShape with HasTypeDelta with HasSuperTypes {

  override def inject(language: Language): Unit = {
    super.inject(language)
    TypeSkeleton.hasSuperTypes.add(language, this)
    TypeSkeleton.typeInstances.add(language, this)
  }

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(shape, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addAlternative(javaGrammar)
  }

  def getJavaGrammar(grammars: LanguageGrammars): BiGrammar
}
