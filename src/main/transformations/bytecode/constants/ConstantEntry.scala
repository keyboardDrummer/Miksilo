package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, TransformationState, MetaObject}
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ConstantPoolItemContentGrammar

trait ConstantEntry extends GrammarTransformation {
  def key: Any
  def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte]

  override def inject(state: TransformationState): Unit = {
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
