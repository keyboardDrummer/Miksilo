package transformations.bytecode.attributes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)

object LineNumberTable extends ParticleWithGrammar {
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new Node(LineNumberTableKey,
    ByteCodeSkeleton.AttributeNameKey -> nameIndex,
    LineNumberTableLines -> lines)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(LineNumberTableKey) = getLineNumberTableBytes
    ByteCodeSkeleton.getState(state).getBytes(LineNumberTableId) = _ => toUTF8ConstantEntry("LineNumberTable")
  }

  def getLineNumberTableBytes(attribute: Node): Seq[Byte] = {
    val entries = LineNumberTable.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getLineNumberTableEntryByteCode(entry: LineNumberRef) =
    shortToBytes(entry.startProgramCounter) ++ shortToBytes(entry.lineNumber)

  def getLineNumberTableEntries(lineNumberTable: Node) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  object LineNumberTableKey

  object LineNumberTableLines

  private object LineNumberTableId
  def lineNumberTableId = new Node(LineNumberTableId)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

  }

  override def description: String = "Defines the line number table attribute. " +
    "This table explains which source code line a particular instruction came from, and can be used to aid in debugging."
}
