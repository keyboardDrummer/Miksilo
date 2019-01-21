package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.Contract
import core.deltas.grammars.{KeyGrammar, LanguageGrammars}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.constants.{ClassInfoConstant, Utf8ConstantDelta}
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.bytecode.readJar.ClassFileParser
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, QualifiedObjectTypeDelta}

object StackMapTableAttributeDelta extends ByteCodeAttribute {

  val entry = Utf8ConstantDelta.create("StackMapTable")

  override def dependencies: Set[Contract] = Set(IntTypeDelta, ByteCodeSkeleton)

  object FrameOffset extends NodeField

  object SameLocals1StackItem extends NodeShape

  object SameLocals1StackItemType extends NodeField

  object AppendFrame extends NodeShape

  object AppendFrameTypes extends NodeField

  object FullFrame extends NodeShape

  object FullFrameLocals extends NodeField

  object FullFrameStack extends NodeField

  object ChopFrame extends NodeShape

  object ChopFrameCount extends NodeField

  object SameFrameKey extends NodeShape

  object Shape extends NodeShape

  object Maps extends NodeField

  def stackMapTable(nameIndex: Int, stackMaps: Seq[Node]) = new Node(Shape,
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

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, Shape, Map(AttributeNameKey -> Utf8ConstantDelta.shape))
    ByteCodeSkeleton.constantReferences.add(language, QualifiedObjectTypeDelta.StackType,
      Map(QualifiedObjectTypeDelta.Name -> ClassInfoConstant.shape))
  }

  def getBytes(compilation: Compilation, attribute: Node): Seq[Byte] = {

    def getFrameByteCode(frame: Node): Seq[Byte] = {
      val offset = StackMapTableAttributeDelta.getFrameOffset(frame)
      frame.shape match {
        case StackMapTableAttributeDelta.ChopFrame =>
            byteToBytes(251 - frame(ChopFrameCount).asInstanceOf[Int]) ++ shortToBytes(offset)
        case StackMapTableAttributeDelta.SameFrameKey =>
          if (offset > 63)
            byteToBytes(251) ++ shortToBytes(offset)
          else
            byteToBytes(offset)
        case StackMapTableAttributeDelta.AppendFrame =>
          val localVerificationTypes = StackMapTableAttributeDelta.getAppendFrameTypes(frame)
          byteToBytes(252 + localVerificationTypes.length - 1) ++
            shortToBytes(offset) ++ localVerificationTypes.flatMap(info => getVerificationInfoBytes(info, compilation))
        case StackMapTableAttributeDelta.SameLocals1StackItem =>
          val _type = StackMapTableAttributeDelta.getSameLocals1StackItemType(frame)
          val code = 64 + offset
          if (code <= 127) {
            byteToBytes(code) ++ getVerificationInfoBytes(_type, compilation)
          } else {
            byteToBytes(247) ++ shortToBytes(offset) ++ getVerificationInfoBytes(_type, compilation)
          }
      }
    }

    val entries = StackMapTableAttributeDelta.getStackMapTableEntries(attribute)
    shortToBytes(entries.length) ++ entries.flatMap(getFrameByteCode)
  }

  def getVerificationInfoBytes(_type: Node, state: Language): Seq[Byte] = {
    _type.shape match {
      case IntTypeDelta.`shape` => hexToBytes("01")
      case LongTypeDelta.`shape` => hexToBytes("04")
      case QualifiedObjectTypeDelta.StackType => hexToBytes("07") ++ shortToBytes(_type(QualifiedObjectTypeDelta.Name).asInstanceOf[Int])
    }
  }

  override def description: String = "Defines the stack map table attribute. Some points in a code attribute instruction list may be jump targets." +
    "For these targets, the stack map table specifies a stack frame." +
    "Each stack from provides information on the current types on the stack and in the local registers." +
    "Given a frame at and the frames before it, all stack and local types are fully defined at the location of that frame."

  override def shape = Shape

  val offsetGrammarKey = KeyGrammar(FrameOffset)
  object StackMapFrameGrammar extends GrammarKey
  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    val verificationGrammar : BiGrammar = getVerificationInfoGrammar(grammars)
    import grammars._
    val offsetGrammar = create(offsetGrammarKey, "," ~~ "offset" ~ ":" ~> integer.as(FrameOffset))

    val sameLocals1StackItemGrammar = (("sameLocalsOneStackItem" ~ offsetGrammar) %
      verificationGrammar.as(SameLocals1StackItemType).indent()).
      asLabelledNode(SameLocals1StackItem)
    val appendFrameGrammar = ("appendFrame" ~ offsetGrammar % verificationGrammar.manyVertical.indent().as(AppendFrameTypes)).
      asLabelledNode(AppendFrame)
    val sameFrameGrammar = "sameFrame" ~ offsetGrammar asNode SameFrameKey
    val chopFrameGrammar = "chopFrame" ~> offsetGrammar ~> ("," ~~ "count" ~ ":" ~> integer.as(ChopFrameCount)) asNode ChopFrame
    val nameGrammar = "name" ~ ":" ~~> find(ConstantPoolIndexGrammar).as(AttributeNameKey)
    val stackMapGrammar: BiGrammar = create(StackMapFrameGrammar, sameFrameGrammar | appendFrameGrammar | sameLocals1StackItemGrammar | chopFrameGrammar)
    val stackMapTableGrammar = (Keyword("StackMapTable", reserved = false) ~ ":" ~~> nameGrammar % stackMapGrammar.manyVertical.indent().as(Maps)).
      asNode(Shape)

    create(Shape, stackMapTableGrammar)
  }

  def getVerificationInfoGrammar(grammars: LanguageGrammars): BiGrammar = {
    val index = grammars.find(ConstantPoolIndexGrammar)
    val basic = grammars.find(IntTypeDelta.shape) |  grammars.find(LongTypeDelta.shape)
    import grammars._
    val objectReference = "class" ~> index.as(QualifiedObjectTypeDelta.Name).asLabelledNode(QualifiedObjectTypeDelta.StackType)

    basic | objectReference
  }

  override def constantPoolKey: String = "StackMapTable"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
