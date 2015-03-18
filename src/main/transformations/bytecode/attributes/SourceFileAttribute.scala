package transformations.bytecode.attributes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.AttributeNameKey
import transformations.bytecode.PrintByteCode._

object SourceFileAttribute extends ParticleWithGrammar {

  object SourceFileAttributeKey

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

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."
}
