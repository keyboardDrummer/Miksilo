package miksilo.modularLanguages.deltas.bytecode.constants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.Keyword
import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.{ConstantPoolItemContentGrammar, HasBytes}

trait ConstantPoolEntry extends DeltaWithGrammar with HasShape with HasBytes {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantEntries.add(language, this)
    ByteCodeSkeleton.hasBytes.add(language, this)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val constantEntryGrammar = getConstantEntryGrammar(grammars)
    import grammars._
    val itemContent = find(ConstantPoolItemContentGrammar)
    itemContent.addAlternative(create(shape, (Keyword(getName, reserved = false) ~~> constantEntryGrammar).asNode(shape)))
  }

  val getName: String
  def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)
}
