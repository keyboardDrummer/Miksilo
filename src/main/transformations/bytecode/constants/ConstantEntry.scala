package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{ParticleWithGrammar, Contract, CompilationState, MetaObject}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolItemContentGrammar

trait ConstantEntry extends ParticleWithGrammar {
  def key: Any
  def getByteCode(constant: MetaObject, state: CompilationState): Seq[Byte]

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes.put(key, (constant: MetaObject) => getByteCode(constant, state))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val itemContent = grammars.find(ConstantPoolItemContentGrammar)
    itemContent.addOption(getGrammar(grammars))
  }
  
  def getGrammar(grammars: GrammarCatalogue): BiGrammar

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies
}
