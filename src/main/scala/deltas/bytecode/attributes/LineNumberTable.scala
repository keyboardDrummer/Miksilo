package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{Contract, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.readJar.ClassFileParser

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)

object LineNumberTable extends ByteCodeAttribute {
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new Node(LineNumberTableKey,
    AttributeNameKey -> nameIndex,
    LineNumberTableLines -> lines)

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).getBytes(LineNumberTableKey) = getLineNumberTableBytes
  }

  def getLineNumberTableBytes(attribute: Node): Seq[Byte] = {
    val entries = LineNumberTable.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getLineNumberTableEntryByteCode(entry: LineNumberRef) =
    shortToBytes(entry.startProgramCounter) ++ shortToBytes(entry.lineNumber)

  def getLineNumberTableEntries(lineNumberTable: Node) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  object LineNumberTableKey extends NodeClass

  object LineNumberTableLines extends NodeField

  override def description: String = "Defines the line number table attribute. " +
    "This table explains which source code line a particular instruction came from, and can be used to aid in debugging."

  override def key = LineNumberTableKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    ("Not implemented" : BiGrammar).asNode(key)
  } // TODO implement. Also figure out why I can't use failure here.

  override def constantPoolKey: String = "LineNumberTable"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
