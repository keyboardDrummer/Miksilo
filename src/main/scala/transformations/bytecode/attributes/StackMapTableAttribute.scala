package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{PartialSelf, CompilationState, Contract}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.readJar.ClassFileParser
import transformations.bytecode.types.ObjectTypeC.ObjectTypeName
import transformations.bytecode.types.{IntTypeC, LongTypeC, ObjectTypeC, TypeSkeleton}

object StackMapTableAttribute extends ByteCodeAttribute {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  object OffsetDelta extends Key

  object SameLocals1StackItem

  object SameLocals1StackItemType

  object AppendFrame

  object AppendFrameTypes

  object FullFrame

  object FullFrameLocals

  object FullFrameStack

  object ChopFrame

  object ChopFrameCount

  object SameFrameKey extends Key

  object StackMapTableKey extends Key

  object StackMapTableMaps

  def stackMapTableId = new Node(StackMapTableId)
  
  private object StackMapTableId

  def stackMapTable(nameIndex: Int, stackMaps: Seq[Node]) = new Node(StackMapTableKey,
    ByteCodeSkeleton.AttributeNameKey -> nameIndex,
    StackMapTableMaps -> stackMaps)

  def getStackMapTableEntries(stackMapTable: Node) = stackMapTable(StackMapTableMaps).asInstanceOf[Seq[Node]]

  def getFrameOffset(frame: Node) = frame(OffsetDelta).asInstanceOf[Int]

  def sameLocals1StackItem(offsetDelta: Int, _type: Node) = new Node(SameLocals1StackItem,
    OffsetDelta -> offsetDelta,
    SameLocals1StackItemType -> _type)

  def getSameLocals1StackItemType(sameLocals1StackItem: Node) = sameLocals1StackItem(SameLocals1StackItemType).asInstanceOf[Node]

  def appendFrame(offset: Int, newLocalTypes: Seq[Node]) = new Node(AppendFrame, OffsetDelta -> offset, AppendFrameTypes -> newLocalTypes)

  def getAppendFrameTypes(appendFrame: Node) = appendFrame(AppendFrameTypes).asInstanceOf[Seq[Node]]

  def sameFrame(offset: Int) = new Node(SameFrameKey, OffsetDelta -> offset)

  object StackMapTableGrammar

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(StackMapTableKey) = (attribute: Node) => getStackMapTableBytes(attribute, state)
    ByteCodeSkeleton.getState(state).getBytes(StackMapTableId) = _ => toUTF8ConstantEntry("StackMapTable")
  }

  def getStackMapTableBytes(attribute: Node, state: CompilationState): Seq[Byte] = {

    def getFrameByteCode(frame: Node): Seq[Byte] = {
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
            shortToBytes(offset) ++ localVerificationTypes.flatMap(info => getVerificationInfoBytes(info, state))
        case StackMapTableAttribute.SameLocals1StackItem =>
          val _type = StackMapTableAttribute.getSameLocals1StackItemType(frame)
          val code = 64 + offset
          if (code <= 127) {
            byteToBytes(code) ++ getVerificationInfoBytes(_type, state)
          } else {
            byteToBytes(247) ++ shortToBytes(offset) ++ getVerificationInfoBytes(_type, state)
          }
      }
    }

    val entries = StackMapTableAttribute.getStackMapTableEntries(attribute)
    shortToBytes(entries.length) ++ entries.flatMap(getFrameByteCode)
  }

  def getVerificationInfoBytes(_type: Node, state: CompilationState): Seq[Byte] = {
    _type.clazz match {
      case IntTypeC.IntTypeKey => hexToBytes("01")
      case LongTypeC.LongTypeKey => hexToBytes("04")
      case ObjectTypeC.ObjectTypeKey => hexToBytes("07") ++ shortToBytes(_type(ObjectTypeName).asInstanceOf[Int])
    }
  }

  override def description: String = "Defines the stack map table attribute. Some points in a code attribute instruction list may be jump targets." +
    "For these targets, the stack map table specifies a stack frame." +
    "Each stack from provides information on the current types on the stack and in the local registers." +
    "Given a frame at and the frames before it, all stack and local types are fully defined at the location of that frame."

  override def key: Key = StackMapTableKey

  object DeltaGrammar
  object StackMapFrameGrammar
  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val deltaGrammar = grammars.create(DeltaGrammar, nodeMap(", delta:" ~> integer, PartialSelf, OffsetDelta))
    val stackMapTableAttributeConstantGrammar = "StackMapTable" ~> produce(stackMapTableId)
    val constantPoolItemContent = grammars.find(ByteCodeSkeleton.ConstantPoolItemContentGrammar)
    constantPoolItemContent.addOption(stackMapTableAttributeConstantGrammar)

    val parseType : BiGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val sameLocals1StackItemGrammar = "same locals, 1 stack item" ~> deltaGrammar % parseType.indent() ^^
      parseMap(SameLocals1StackItem, PartialSelf, SameLocals1StackItemType)
    val appendFrameGrammar = "append frame" ~> deltaGrammar % parseType.manyVertical.indent() ^^ //TODO idee: in LabelledLocations dit opnieuw definieren.
      parseMap(AppendFrame, PartialSelf, AppendFrameTypes)
    val sameFrameGrammar = nodeMap("same frame" ~> deltaGrammar, SameFrameKey, PartialSelf)
    val stackMapGrammar: BiGrammar = grammars.create(StackMapFrameGrammar, sameFrameGrammar | appendFrameGrammar | sameLocals1StackItemGrammar)
    val stackMapTableGrammar = "stackMap nameIndex:" ~> integer % stackMapGrammar.manyVertical.indent() ^^
      parseMap(StackMapTableKey, ByteCodeSkeleton.AttributeNameKey, StackMapTableMaps)

    grammars.create(StackMapTableGrammar, stackMapTableGrammar)
  }

  override def constantPoolKey: String = "StackMapTable"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
