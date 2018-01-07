package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape, NodeField}
import core.deltas.{Contract, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.readJar.ClassFileParser
import deltas.bytecode.readJar.ClassFileParser._

object SourceFileAttribute extends ByteCodeAttribute {

  object SourceFileAttributeKey extends NodeShape

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

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."

  override def key = SourceFileAttributeKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    ("NotImplemented" : BiGrammar).asNode(key)
  } // TODO implement.

  override def constantPoolKey: String = "SourceFile"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ParseShort.
    map(index => new Node(SourceFileAttributeKey, SourceFileFileNameIndex -> index))
}
