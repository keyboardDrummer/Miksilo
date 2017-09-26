package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolItemContentGrammar

trait ConstantEntry extends DeltaWithGrammar {

  object Clazz extends NodeClass
  val key = Clazz //TODO inlinen
  def getByteCode(constant: Node, state: Language): Seq[Byte]

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes.put(key, (constant: Node) => getByteCode(constant, state))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val itemContent = grammars.find(ConstantPoolItemContentGrammar)
    itemContent.addOption(grammars.create(key, getName ~~> getConstantEntryGrammar(grammars)))
  }

  def getName: BiGrammar
  def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies
}
