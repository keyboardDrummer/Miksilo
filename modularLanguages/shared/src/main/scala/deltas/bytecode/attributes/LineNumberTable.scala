package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.Contract
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeField, NodeShape}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.readJar.ClassFileParser

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)

object LineNumberTable extends ByteCodeAttribute {
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new Node(LineNumberTableKey,
    AttributeNameKey -> nameIndex,
    LineNumberTableLines -> lines)

  def getBytes(compilation: Compilation, attribute: Node): Seq[Byte] = {
    val entries = LineNumberTable.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getLineNumberTableEntryByteCode(entry: LineNumberRef) =
    shortToBytes(entry.startProgramCounter) ++ shortToBytes(entry.lineNumber)

  def getLineNumberTableEntries(lineNumberTable: Node) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  object LineNumberTableKey extends NodeShape

  object LineNumberTableLines extends NodeField

  override def description: String = "Defines the line number table attribute. " +
    "This table explains which source code line a particular instruction came from, and can be used to aid in debugging."

  override def shape = LineNumberTableKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    ("NotImplemented" : BiGrammar).asNode(shape)
  } // TODO implement. Also figure out why I can't use failure here.

  override def constantPoolKey: String = "LineNumberTable"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
