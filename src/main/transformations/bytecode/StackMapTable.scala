package transformations.bytecode

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject}

object StackMapTable extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  object OffsetDelta

  object SameLocals1StackItem

  object SameLocals1StackItemType

  object AppendFrame

  object AppendFrameTypes

  object FullFrame

  object FullFrameLocals

  object FullFrameStack

  object ChopFrame

  object ChopFrameCount

  object SameFrameKey

  object StackMapTableKey

  object StackMapTableMaps

  object StackMapTableId

  def stackMapTable(nameIndex: Int, stackMaps: Seq[MetaObject]) = new MetaObject(StackMapTableKey) {
    data.put(ByteCodeSkeleton.AttributeNameKey, nameIndex)
    data.put(StackMapTableMaps, stackMaps)
  }

  def getStackMapTableEntries(stackMapTable: MetaObject) = stackMapTable(StackMapTableMaps).asInstanceOf[Seq[MetaObject]]

  def getFrameOffset(frame: MetaObject) = frame(OffsetDelta).asInstanceOf[Int]

  def sameFrameLocals1StackItem(offsetDelta: Int, _type: MetaObject) = new MetaObject(SameLocals1StackItem) {
    data.put(OffsetDelta, offsetDelta)
    data.put(SameLocals1StackItemType, _type)
  }

  def getSameLocals1StackItemType(sameLocals1StackItem: MetaObject) = sameLocals1StackItem(SameLocals1StackItemType).asInstanceOf[MetaObject]

  def appendFrame(offset: Int, newLocalTypes: Seq[MetaObject]) = new MetaObject(AppendFrame) {
    data.put(OffsetDelta, offset)
    data.put(AppendFrameTypes, newLocalTypes)
  }

  def getAppendFrameTypes(appendFrame: MetaObject) = appendFrame(AppendFrameTypes).asInstanceOf[Seq[MetaObject]]

  def sameFrame(offset: Int) = new MetaObject(SameFrameKey) {
    data.put(OffsetDelta, offset)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = ???
}
