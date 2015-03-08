package transformations.bytecode.constants

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._

object ClassRefConstant extends ConstantEntry {

  object ClassRefKey

  object ClassRefName

  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }

  def getNameIndex(classRef: MetaObject) = classRef(ClassRefName).asInstanceOf[Int]

  override def key: Any = ClassRefKey

  override def getByteCode(constant: MetaObject, state: CompilationState): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(getNameIndex(constant))
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "class reference:" ~~> integer ^^ parseMap(ClassRefKey, ClassRefName)

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."
}
