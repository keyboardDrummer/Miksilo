package transformations.bytecode.attributes

import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)

object LineNumberTable extends ParticleWithGrammar {
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new MetaObject(LineNumberTableKey,
    ByteCodeSkeleton.AttributeNameKey -> nameIndex,
    LineNumberTableLines -> lines)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(LineNumberTableKey) = getLineNumberTableBytes
    ByteCodeSkeleton.getState(state).getBytes(LineNumberTableId) = _ => toUTF8ConstantEntry("LineNumberTable")
  }

  def getLineNumberTableBytes(attribute: MetaObject): Seq[Byte] = {
    val entries = LineNumberTable.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getLineNumberTableEntryByteCode(entry: LineNumberRef) =
    shortToBytes(entry.startProgramCounter) ++ shortToBytes(entry.lineNumber)

  def getLineNumberTableEntries(lineNumberTable: MetaObject) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  object LineNumberTableKey

  object LineNumberTableLines

  private object LineNumberTableId
  def lineNumberTableId = new MetaObject(LineNumberTableId)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

  }

  override def description: String = "Defines the line number table attribute. " +
    "This table explains which source code line a particular instruction came from, and can be used to aid in debugging."
}
