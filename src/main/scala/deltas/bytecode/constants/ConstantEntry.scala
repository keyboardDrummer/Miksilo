package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeShape}
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.ConstantPoolItemContentGrammar

trait ConstantEntry extends DeltaWithGrammar {

  def key: NodeShape

  def getByteCode(constant: Node, state: Language): Seq[Byte]

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.getRegistry(language).constantEntries.add(this)
    ByteCodeSkeleton.getRegistry(language).getBytes.put(key, (constant: Node) => getByteCode(constant, language))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val constantEntryGrammar = getConstantEntryGrammar(grammars)
    import grammars._
    val itemContent = find(ConstantPoolItemContentGrammar)
    itemContent.addOption(create(key, (getName ~~> constantEntryGrammar).asNode(key)))
  }

  def getName: BiGrammar
  def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)
}
