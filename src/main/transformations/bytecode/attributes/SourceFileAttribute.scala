package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.AttributeNameKey
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.readJar.ClassFileParser
import ClassFileParser._

object SourceFileAttribute extends ByteCodeAttribute {

  object SourceFileAttributeKey extends Key

  object SourceFileFileNameIndex

  def sourceFile(nameIndex: Int, fileNameIndex: Int): Node = new Node(SourceFileAttributeKey,
    SourceFileFileNameIndex -> fileNameIndex,
    AttributeNameKey -> nameIndex)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(SourceFileAttributeKey) = getSourceFileBytes
  }

  def getSourceFileBytes(sourceFile: Node) = {
    shortToBytes(getSourceFileFileNameIndex(sourceFile))
  }

  def getSourceFileFileNameIndex(sourceFile: Node) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."

  override def key: Key = SourceFileAttributeKey

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "Not implemented" ^^ parseMap(key) // TODO implement.

  override def constantPoolKey: String = "SourceFile"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ParseShort.
    map(index => new Node(SourceFileAttributeKey, SourceFileFileNameIndex -> index))
}
