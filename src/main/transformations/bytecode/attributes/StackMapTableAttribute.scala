package transformations.bytecode.attributes

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.types.TypeSkeleton

object StackMapTableAttribute extends ParticleWithGrammar {

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

  def stackMapTableId = new MetaObject(StackMapTableId)
  
  private object StackMapTableId

  def stackMapTable(nameIndex: Int, stackMaps: Seq[MetaObject]) = new MetaObject(StackMapTableKey) {
    data.put(ByteCodeSkeleton.AttributeNameKey, nameIndex)
    data.put(StackMapTableMaps, stackMaps)
  }

  def getStackMapTableEntries(stackMapTable: MetaObject) = stackMapTable(StackMapTableMaps).asInstanceOf[Seq[MetaObject]]

  def getFrameOffset(frame: MetaObject) = frame(OffsetDelta).asInstanceOf[Int]

  def sameLocals1StackItem(offsetDelta: Int, _type: MetaObject) = new MetaObject(SameLocals1StackItem) {
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

  object StackMapTableGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val stackMapTableAttributeConstantGrammar = "StackMapTable" ~> produce(stackMapTableId)
    val constantPoolItemContent = grammars.find(ByteCodeSkeleton.ConstantPoolItemContentGrammar)
    constantPoolItemContent.addOption(stackMapTableAttributeConstantGrammar)

    val parseType : BiGrammar = grammars.find(TypeSkeleton.TypeGrammar)
    val sameLocals1StackItemGrammar = "same locals, 1 stack item, delta:" ~> integer % parseType.indent() ^^
      parseMap(SameLocals1StackItem, OffsetDelta, SameLocals1StackItemType)
    val appendFrameGrammar = "append frame, delta:" ~> integer % parseType.manyVertical.indent() ^^
      parseMap(AppendFrame, OffsetDelta, AppendFrameTypes)
    val sameFrameGrammar = "same frame, delta:" ~> integer ^^ parseMap(SameFrameKey, OffsetDelta)
    val stackMapGrammar: BiGrammar = sameFrameGrammar | appendFrameGrammar | sameLocals1StackItemGrammar
    val stackMapTableGrammar = "sm nameIndex:" ~> integer % stackMapGrammar.manyVertical.indent() ^^
      parseMap(StackMapTableKey, ByteCodeSkeleton.AttributeNameKey, StackMapTableMaps)

    grammars.find(ByteCodeSkeleton.AttributeGrammar).addOption(grammars.create(StackMapTableGrammar, stackMapTableGrammar))
  }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(StackMapTableKey) = (attribute: MetaObject) => getStackMapTableBytes(attribute, state)
    ByteCodeSkeleton.getState(state).getBytes(StackMapTableId) = _ => toUTF8ConstantEntry("StackMapTable")
  }

  def getStackMapTableBytes(attribute: MetaObject, state: CompilationState): Seq[Byte] = {

    def getFrameByteCode(frame: MetaObject): Seq[Byte] = {
      val offset = StackMapTableAttribute.getFrameOffset(frame)
      frame.clazz match {
        case StackMapTableAttribute.SameFrameKey =>
          if (offset > 63)
            byteToBytes(251) ++ shortToBytes(offset)
          else
            byteToBytes(offset)
        case StackMapTableAttribute.AppendFrame =>
          val localVerificationTypes = StackMapTableAttribute.getAppendFrameTypes(frame)
          byteToBytes(252 + localVerificationTypes.length - 1) ++
            shortToBytes(offset) ++ localVerificationTypes.flatMap(info => TypeSkeleton.getVerificationInfoBytes(info, state))
        case StackMapTableAttribute.SameLocals1StackItem =>
          val _type = StackMapTableAttribute.getSameLocals1StackItemType(frame)
          val code = 64 + offset
          if (code <= 127) {
            byteToBytes(code) ++ TypeSkeleton.getVerificationInfoBytes(_type, state)
          } else {
            byteToBytes(247) ++ shortToBytes(offset) ++ TypeSkeleton.getVerificationInfoBytes(_type, state)
          }
      }
    }

    val entries = StackMapTableAttribute.getStackMapTableEntries(attribute)
    shortToBytes(entries.length) ++ entries.flatMap(getFrameByteCode)
  }

  override def description: String = "Defines the stack map table attribute. Some points in a code attribute instruction list may be jump targets." +
    "For these targets, the stack map table specifies a stack frame." +
    "Each stack from provides information on the current types on the stack and in the local registers." +
    "Given a frame at and the frames before it, all stack and local types are fully defined at the location of that frame."
}
