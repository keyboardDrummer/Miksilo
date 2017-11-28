package deltas.bytecode

import core.bigrammar.BiGrammar
import core.document.Empty
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node._
import deltas.bytecode.ByteCodeFieldInfo.FieldInfoWrapper
import deltas.bytecode.ByteCodeMethodInfo.ByteCodeMethodInfoWrapper
import deltas.bytecode.attributes.{AttributeNameKey, ByteCodeAttribute}
import deltas.bytecode.constants.{ClassInfoConstant, ConstantEntry}
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.javac.classes.ConstantPool

import scala.collection.mutable

object ByteCodeSkeleton extends DeltaWithGrammar with WithLanguageRegistry {

  implicit class ClassFile[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def constantPool: ConstantPool = node(ClassConstantPool).asInstanceOf[ConstantPool]
    def constantPool_=(constantPool: ConstantPool): Unit = node(ClassConstantPool) = constantPool

    def parentIndex: Int = node(ClassParentIndex).asInstanceOf[Int]
    def parentIndex_=(index: Int): Unit = node(ClassParentIndex) = index

    def classInfoIndex: Int = node(ClassNameIndexKey).asInstanceOf[Int]
    def classInfoIndex_=(index: Int): Unit = node(ClassNameIndexKey) = index

    def interfaceIndices: Seq[Int] = node(ClassInterfaces).asInstanceOf[Seq[Int]]
    def interfaceIndices_=(indices: Seq[Int]): Unit = node(ClassInterfaces) = indices

    def fields: Seq[FieldInfoWrapper[T]] = NodeWrapper.wrapList(node(ClassFields).asInstanceOf[Seq[T]])

    def attributes: Seq[T] = node(ClassAttributes).asInstanceOf[Seq[T]]

    def methods: Seq[ByteCodeMethodInfoWrapper[T]] = NodeWrapper.wrapList(node(Methods).asInstanceOf[Seq[T]])
  }

  def getAttributeNameIndex(attribute: Node) = attribute(AttributeNameKey).asInstanceOf[Int]

  def createRegistry = new Registry()

  def clazz(name: Int, parent: Int, constantPool: ConstantPool, methods: Seq[Node], interfaces: Seq[Int] = Seq(),
            classFields: Seq[Node] = Seq(), attributes: Seq[Node] = Seq()) = new Node(Clazz,
    Methods ->  methods,
    ClassNameIndexKey ->  name,
    ClassParentIndex ->  parent,
    ClassConstantPool ->  constantPool,
    ClassInterfaces ->  interfaces,
    ClassFields ->  classFields,
    ClassAttributes ->  attributes
  )

  override def dependencies: Set[Contract] = Set.empty

  class Registry {
    val getBytes = new ClassRegistry[Node => Seq[Byte]]
    val attributes = new mutable.HashMap[String, ByteCodeAttribute]
    val constantReferences = new ClassRegistry[Map[NodeField, NodeClass]]
    val constantEntries = mutable.Set.empty[ConstantEntry]
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(Clazz, Map(
      //TODO add with seq support //ClassInterfaces -> ClassRefConstant.key,
      ClassParentIndex -> ClassInfoConstant.key,
      ClassNameIndexKey -> ClassInfoConstant.key))
  }

  object Clazz extends NodeClass

  object Methods extends NodeField

  object ClassNameIndexKey extends NodeField

  object ClassParentIndex extends NodeField

  object ClassConstantPool extends NodeField

  object ClassInterfaces extends NodeField

  object ClassFields extends NodeField

  object ClassAttributes extends NodeField

  object ConstantPoolItemContentGrammar extends GrammarKey

  object AttributeGrammar extends GrammarKey
  object MembersGrammar extends GrammarKey
  object AttributesGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val constantPool: BiGrammar = getConstantPoolGrammar(grammars)
    import grammars._
    val constantIndexGrammar = create(ConstantPoolIndexGrammar, integer)
    val attributeGrammar: BiGrammar = create(AttributeGrammar)
    val interfacesGrammar: BiGrammar = "with interfaces:" ~~> (constantIndexGrammar *).inParenthesis
    val classIndexGrammar: BiGrammar = "class" ~~> constantIndexGrammar
    val parseIndexGrammar: BiGrammar = "extends" ~~> constantIndexGrammar
    val attributesGrammar = create(AttributesGrammar, attributeGrammar.manyVertical)
    val membersGrammar = create(MembersGrammar, print(Empty))
    val bodyGrammar = "{" % (membersGrammar % attributesGrammar.as(ClassAttributes)).indent() % "}"
    val classGrammar = create(Clazz,
      (classIndexGrammar.as(ClassNameIndexKey) ~~ parseIndexGrammar.as(ClassParentIndex) ~~ interfacesGrammar.as(ClassInterfaces) %
        constantPool % bodyGrammar).asNode(Clazz))

    find(BodyGrammar).inner = classGrammar
  }

  object ConstantPoolGrammar extends GrammarKey

  def getConstantPoolGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val constantPoolItemContent = create(ConstantPoolItemContentGrammar)
    val entries = constantPoolItemContent.manyVertical.indent()
    val result = "Constant pool:" %> entries ^^ (
      entries => new ConstantPool(entries.asInstanceOf[Seq[Any]]),
      constantPool => Some(constantPool.asInstanceOf[ConstantPool].constants.toSeq))
    create(ConstantPoolGrammar, result.as(ClassConstantPool))
  }

  override def description: String = "Defines a skeleton for bytecode."
}




