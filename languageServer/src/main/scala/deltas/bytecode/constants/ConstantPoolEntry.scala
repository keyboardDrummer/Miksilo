package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.{ConstantPoolItemContentGrammar, HasBytes}

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
    itemContent.addAlternative(create(shape, (getName ~~> constantEntryGrammar).asNode(shape)))
  }

  val getName: BiGrammar
  def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)
}
