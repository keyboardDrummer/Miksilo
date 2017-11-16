package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}
import core.deltas.{Contract, DeltaWithGrammar, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.ConstantPoolItemContentGrammar

trait ConstantEntry extends DeltaWithGrammar {

  def key: NodeClass
  def getByteCode(constant: Node, state: Language): Seq[Byte]

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).getBytes.put(key, (constant: Node) => getByteCode(constant, state))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val constantEntryGrammar = getConstantEntryGrammar(grammars)
    import grammars._
    val itemContent = find(ConstantPoolItemContentGrammar)
    itemContent.addOption(create(key, (getName ~~> constantEntryGrammar).asNode(key)))
  }

  def getName: BiGrammar
  def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies
}
