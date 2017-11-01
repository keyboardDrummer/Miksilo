package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{Contract, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.readJar.ClassFileParser
import transformations.bytecode.readJar.ClassFileParser._

object SourceFileAttribute extends ByteCodeAttribute {

  object SourceFileAttributeKey extends NodeClass

  object SourceFileFileNameIndex extends NodeField

  def sourceFile(nameIndex: Int, fileNameIndex: Int): Node = new Node(SourceFileAttributeKey,
    SourceFileFileNameIndex -> fileNameIndex,
    AttributeNameKey -> nameIndex)

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).getBytes(SourceFileAttributeKey) = getSourceFileBytes
  }

  def getSourceFileBytes(sourceFile: Node) = {
    shortToBytes(getSourceFileFileNameIndex(sourceFile))
  }

  def getSourceFileFileNameIndex(sourceFile: Node) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."

  override def key = SourceFileAttributeKey

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    import grammars._
    ("Not implemented" : BiGrammar).asNode(key)
  } // TODO implement.

  override def constantPoolKey: String = "SourceFile"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ParseShort.
    map(index => new Node(SourceFileAttributeKey, SourceFileFileNameIndex -> index))
}
