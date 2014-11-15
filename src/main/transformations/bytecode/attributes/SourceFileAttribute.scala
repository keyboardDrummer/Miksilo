package transformations.bytecode.attributes

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton.AttributeNameKey

trait SourceFileAttribute {

  object SourceFileAttribute

  object SourceFileFileNameIndex

  object SourceFileId

  def sourceFile(nameIndex: Int, fileNameIndex: Int): MetaObject = new MetaObject(SourceFileAttribute) {
    data.put(SourceFileFileNameIndex, fileNameIndex)
    data.put(AttributeNameKey, nameIndex)
  }

  def getSourceFileFileNameIndex(sourceFile: MetaObject) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]
}
