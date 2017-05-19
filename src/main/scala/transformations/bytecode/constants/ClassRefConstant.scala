package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object ClassRefConstant extends ConstantEntry {

  object ClassRefKey extends Key

  object ClassRefName extends Key

  def classRef(classRefNameIndex: Int): Node = new Node(ClassRefKey, ClassRefName -> classRefNameIndex)

  def getNameIndex(classRef: Node) = classRef(ClassRefName).asInstanceOf[Int]

  override def key: Any = ClassRefKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(7) ++ shortToBytes(getNameIndex(constant))
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = "class reference:" ~~> integer asNode(ClassRefKey, ClassRefName)

  override def description: String = "Adds a new type of constant named the class reference. " +
    "It only contains an index pointing to a string constant that contains the name of the class."
}
