package transformations.bytecode.attributes

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.constants.ConstantEntry

object CodeConstantEntry extends ConstantEntry
{
  object CodeAttributeId

  def entry = new MetaObject(CodeAttributeId)

  override def key: Any = CodeAttributeId

  override def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte] = {
    PrintByteCode.toUTF8ConstantEntry("Code")
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "Code" ~> produce(entry)
}
