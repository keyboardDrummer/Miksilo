package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.{Key, Node, NodeClass, NodeField}
import core.particles.{Contract, Language}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.{ClassInfoConstant, Utf8ConstantDelta}
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.readJar.ClassFileParser
import transformations.bytecode.types.ObjectTypeDelta.ObjectStackType
import transformations.bytecode.types.{IntTypeC, LongTypeC, ObjectTypeDelta}

object StackMapTableAttribute extends ByteCodeAttribute {

  val entry = Utf8ConstantDelta.create("StackMapTable")
  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  object FrameOffset extends NodeField

  object SameLocals1StackItem extends NodeClass

  object SameLocals1StackItemType extends NodeField

  object AppendFrame extends NodeClass

  object AppendFrameTypes extends NodeField

  object FullFrame extends NodeClass

  object FullFrameLocals extends NodeField

  object FullFrameStack extends NodeField

  object ChopFrame extends NodeClass

  object ChopFrameCount extends NodeField

  object SameFrameKey extends NodeClass

  object Clazz extends NodeClass

  object Maps extends NodeField

  def stackMapTable(nameIndex: Int, stackMaps: Seq[Node]) = new Node(Clazz,
    AttributeNameKey -> nameIndex,
    Maps -> stackMaps)

  def getStackMapTableEntries(stackMapTable: Node) = stackMapTable(Maps).asInstanceOf[Seq[Node]]

  def getFrameOffset(frame: Node) = frame(FrameOffset).asInstanceOf[Int]

  def sameLocals1StackItem(frameOffset: Int, _type: Node) = new Node(SameLocals1StackItem,
    FrameOffset -> frameOffset,
    SameLocals1StackItemType -> _type)

  def getSameLocals1StackItemType(sameLocals1StackItem: Node) = sameLocals1StackItem(SameLocals1StackItemType).asInstanceOf[Node]

  def appendFrame(offset: Int, newLocalTypes: Seq[Node]) = new Node(AppendFrame, FrameOffset -> offset,
    AppendFrameTypes -> newLocalTypes)

  def getAppendFrameTypes(appendFrame: Node) = appendFrame(AppendFrameTypes).asInstanceOf[Seq[Node]]

  def sameFrame(offset: Int) = new Node(SameFrameKey, FrameOffset -> offset)

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).getBytes(Clazz) = (attribute: Node) => getStackMapTableBytes(attribute, state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(Clazz, Map(AttributeNameKey -> Utf8ConstantDelta.key))
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(ObjectStackType, Map(ObjectTypeDelta.Name -> ClassInfoConstant.key))
  }

  def getStackMapTableBytes(attribute: Node, state: Language): Seq[Byte] = {

    def getFrameByteCode(frame: Node): Seq[Byte] = {
      val offset = StackMapTableAttribute.getFrameOffset(frame)
      frame.clazz match {
        case StackMapTableAttribute.ChopFrame =>
            byteToBytes(251 - frame(ChopFrameCount).asInstanceOf[Int]) ++ shortToBytes(offset)
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

  def getVerificationInfoBytes(_type: Node, state: Language): Seq[Byte] = {
    _type.clazz match {
      case IntTypeC.key => hexToBytes("01")
      case LongTypeC.key => hexToBytes("04")
      case ObjectTypeDelta.ObjectStackType => hexToBytes("07") ++ shortToBytes(_type(ObjectTypeDelta.Name).asInstanceOf[Int])
    }
  }

  override def description: String = "Defines the stack map table attribute. Some points in a code attribute instruction list may be jump targets." +
    "For these targets, the stack map table specifies a stack frame." +
    "Each stack from provides information on the current types on the stack and in the local registers." +
    "Given a frame at and the frames before it, all stack and local types are fully defined at the location of that frame."

  override def key: Key = Clazz

  val offsetGrammarKey = KeyGrammar(FrameOffset)
  object StackMapFrameGrammar extends Key
  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val offsetGrammar = grammars.create(offsetGrammarKey, ", offset:" ~> integer as FrameOffset)

    val verificationGrammar : BiGrammar = getVerificationInfoGrammar(grammars)
    val sameLocals1StackItemGrammar = (("same locals, 1 stack item" ~ offsetGrammar) %
      verificationGrammar.as(SameLocals1StackItemType).indent()).
      asLabelledNode(grammars, SameLocals1StackItem)
    val appendFrameGrammar = ("append frame" ~ offsetGrammar % verificationGrammar.manyVertical.indent().as(AppendFrameTypes)).
      asLabelledNode(grammars, AppendFrame)
    val sameFrameGrammar = "same frame" ~ offsetGrammar asNode SameFrameKey
    val chopFrameGrammar = "chop frame" ~> offsetGrammar ~> (", count = " ~> integer.as(ChopFrameCount)) asNode ChopFrame
    val nameGrammar = "name:" ~~> grammars.find(ConstantPoolIndexGrammar).as(AttributeNameKey)
    val stackMapGrammar: BiGrammar = grammars.create(StackMapFrameGrammar, sameFrameGrammar | appendFrameGrammar | sameLocals1StackItemGrammar | chopFrameGrammar)
    val stackMapTableGrammar = ("StackMapTable:" ~~> nameGrammar % stackMapGrammar.manyVertical.indent().as(Maps)).
      asNode(Clazz)

    grammars.create(Clazz, stackMapTableGrammar)
  }

  def getVerificationInfoGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val index = grammars.find(ConstantPoolIndexGrammar)
    val basic = grammars.find(IntTypeC.key) |  grammars.find(LongTypeC.key)
    val objectReference = "class" ~> index.as(ObjectTypeDelta.Name).asLabelledNode(grammars, ObjectStackType)

    basic | objectReference
  }

  override def constantPoolKey: String = "StackMapTable"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
