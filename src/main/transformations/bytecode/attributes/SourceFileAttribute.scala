package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.AttributeNameKey
import transformations.bytecode.PrintByteCode._

object SourceFileAttribute extends ByteCodeAttribute {

  object SourceFileAttributeKey extends Key

  object SourceFileFileNameIndex

  private object SourceFileId

  def sourceFileId = new Node(SourceFileId)

  def sourceFile(nameIndex: Int, fileNameIndex: Int): Node = new Node(SourceFileAttributeKey,
    SourceFileFileNameIndex -> fileNameIndex,
    AttributeNameKey -> nameIndex)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(SourceFileAttributeKey) = getSourceFileBytes
    ByteCodeSkeleton.getState(state).getBytes(SourceFileId) = _ => toUTF8ConstantEntry("SourceFile")
  }

  def getSourceFileBytes(sourceFile: Node) = {
    shortToBytes(getSourceFileFileNameIndex(sourceFile))
  }

  def getSourceFileFileNameIndex(sourceFile: Node) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."

  override def key: Key = SourceFileAttributeKey

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = null

  override def constantPoolKey: String = "SourceFile"
}
