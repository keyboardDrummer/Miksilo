package transformations.bytecode.constants

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode._

object NameAndType extends ConstantEntry {

  object NameAndTypeKey

  object NameAndTypeName

  object NameAndTypeType

  def nameAndType(nameIndex: Int, typeIndex: Int): Node = new Node(NameAndTypeKey,
    NameAndTypeName -> nameIndex,
    NameAndTypeType -> typeIndex)

  def getNameAndTypeName(nameAndType: Node) = nameAndType(NameAndTypeName).asInstanceOf[Int]

  def getTypeIndex(nameAndType: Node) = nameAndType(NameAndTypeType).asInstanceOf[Int]

  override def key: Any = NameAndTypeKey

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    byteToBytes(12) ++ shortToBytes(getNameAndTypeName(constant)) ++
      shortToBytes(getTypeIndex(constant))
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "name and type:" ~~> (integer <~ ":") ~ integer ^^
    parseMap(NameAndTypeKey, NameAndTypeName, NameAndTypeType)

  override def description: String = "Defines the name and type constant, which contains a name and a field or method descriptor."
}
