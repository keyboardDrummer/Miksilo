package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._

object NameAndType extends ConstantEntry {

  object NameAndTypeKey extends Key

  object NameAndTypeName extends Key

  object NameAndTypeType extends Key

  def nameAndType(nameIndex: Int, typeIndex: Int): Node = new Node(NameAndTypeKey,
    NameAndTypeName -> nameIndex,
    NameAndTypeType -> typeIndex)

  def getName(nameAndType: Node): Int = nameAndType(NameAndTypeName).asInstanceOf[Int]

  def getTypeIndex(nameAndType: Node): Int = nameAndType(NameAndTypeType).asInstanceOf[Int]

  override def key: Any = NameAndTypeKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(12) ++ shortToBytes(getName(constant)) ++
      shortToBytes(getTypeIndex(constant))
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = ("name and type:" ~~> (integer <~ ":") ~ integer).
    asNode(NameAndTypeKey, NameAndTypeName, NameAndTypeType)

  override def description: String = "Defines the name and type constant, which contains a name and a field or method descriptor."
}
