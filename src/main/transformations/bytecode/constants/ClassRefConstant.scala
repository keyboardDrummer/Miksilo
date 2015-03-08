package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.PrintByteCode._

object ClassRefConstant extends ConstantEntry {

  object ClassRefKey

  object ClassRefName

  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }

  def getNameIndex(classRef: MetaObject) = classRef(ClassRefName).asInstanceOf[Int]

  override def key: Any = ClassRefKey

  override def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(getNameIndex(constant))
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "class reference:" ~~> integer ^^ parseMap(ClassRefKey, ClassRefName)

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."
}
