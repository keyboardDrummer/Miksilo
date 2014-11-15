package transformations.bytecode.attributes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject}
import transformations.bytecode.ByteCodeSkeleton

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)

object LineNumberTable extends GrammarTransformation {
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new MetaObject(LineNumberTableKey) {
    data.put(ByteCodeSkeleton.AttributeNameKey, nameIndex)
    data.put(LineNumberTableLines, lines)
  }

  def getLineNumberTableEntries(lineNumberTable: MetaObject) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  object LineNumberTableKey

  object LineNumberTableLines

  object LineNumberTableId

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

  }
}
