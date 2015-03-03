package transformations.bytecode.attributes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.AttributeNameKey
import transformations.bytecode.PrintByteCode._

object SourceFileAttribute extends GrammarTransformation {

  object SourceFileAttribute

  object SourceFileFileNameIndex

  private object SourceFileId

  def sourceFileId = new MetaObject(SourceFileId)

  def sourceFile(nameIndex: Int, fileNameIndex: Int): MetaObject = new MetaObject(SourceFileAttribute) {
    data.put(SourceFileFileNameIndex, fileNameIndex)
    data.put(AttributeNameKey, nameIndex)
  }

  override def inject(state: TransformationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(SourceFileAttribute) = getSourceFileBytes
    ByteCodeSkeleton.getState(state).getBytes(SourceFileId) = _ => toUTF8ConstantEntry("SourceFile")
  }

  def getSourceFileBytes(sourceFile: MetaObject) = {
    shortToBytes(getSourceFileFileNameIndex(sourceFile))
  }

  def getSourceFileFileNameIndex(sourceFile: MetaObject) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

  }
}
