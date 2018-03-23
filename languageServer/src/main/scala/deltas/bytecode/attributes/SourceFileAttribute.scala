package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.Contract
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
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

  def getBytes(compilation: Compilation, sourceFile: Node) = {
    shortToBytes(getSourceFileFileNameIndex(sourceFile))
  }

  def getSourceFileFileNameIndex(sourceFile: Node) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."

  override def shape = SourceFileAttributeKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    ("NotImplemented" : BiGrammar).asNode(shape)
  } // TODO implement.

  override def constantPoolKey: String = "SourceFile"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ParseShort.
    map(index => new Node(SourceFileAttributeKey, SourceFileFileNameIndex -> index))
}
