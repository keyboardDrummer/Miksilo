package transformations.bytecode.constants

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolItemContentGrammar

trait ConstantEntry extends ParticleWithGrammar {
  def key: Any
  def getByteCode(constant: Node, state: CompilationState): Seq[Byte]

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes.put(key, (constant: Node) => getByteCode(constant, state))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val itemContent = grammars.find(ConstantPoolItemContentGrammar)
    itemContent.addOption(getGrammar(grammars))
  }
  
  def getGrammar(grammars: GrammarCatalogue): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies
}
