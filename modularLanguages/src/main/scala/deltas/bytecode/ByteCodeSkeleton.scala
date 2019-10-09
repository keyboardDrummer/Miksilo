package deltas.bytecode

import core.document.Empty
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeFieldInfo.FieldInfoWrapper
import deltas.bytecode.ByteCodeMethodInfo.MethodInfo
import deltas.bytecode.attributes.{AttributeNameKey, ByteCodeAttribute}
import deltas.bytecode.constants.{ClassInfoConstant, ConstantPoolEntry}
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.javac.classes.ConstantPool

import scala.collection.mutable

object ByteCodeSkeleton extends DeltaWithGrammar {

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
    def attributes_=(value: Seq[T]) = node(ClassAttributes) = value

    def methods: Seq[MethodInfo[T]] = NodeWrapper.wrapList(node(Methods).asInstanceOf[Seq[T]])
  }

  def getAttributeNameIndex(attribute: Node) = attribute(AttributeNameKey).asInstanceOf[Int]

  def neww(name: Int, parent: Int, constantPool: ConstantPool, methods: Seq[Node], interfaces: Seq[Int] = Seq(),
           classFields: Seq[Node] = Seq(), attributes: Seq[Node] = Seq()) = new Node(Shape,
    Methods ->  methods,
    ClassNameIndexKey ->  name,
    ClassParentIndex ->  parent,
    ClassConstantPool ->  constantPool,
    ClassInterfaces ->  interfaces,
    ClassFields ->  classFields,
    ClassAttributes ->  attributes
  )

  override def dependencies: Set[Contract] = Set.empty

  trait HasBytes {
    def getBytes(compilation: Compilation, node: Node): Seq[Byte]
  }

  def getBytes(compilation: Compilation, node: Node): Seq[Byte] = hasBytes(compilation, node.shape).getBytes(compilation, node)

  val hasBytes = new ShapeProperty[HasBytes]
  val constantEntries = new ShapeProperty[ConstantPoolEntry]
  val attributesByName = new Property[mutable.HashMap[String, ByteCodeAttribute]](mutable.HashMap.empty)
  val constantReferences = new ShapeProperty[Map[NodeField, NodeShape]]

  override def inject(language: Language): Unit = {
    super.inject(language)
    constantReferences.add(language, Shape, Map(
      //TODO add with seq support //ClassInterfaces -> ClassRefConstant.key,
      ClassParentIndex -> ClassInfoConstant.shape,
      ClassNameIndexKey -> ClassInfoConstant.shape))
  }

  object Shape extends NodeShape

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
    val interfacesGrammar: BiGrammar = "with" ~ ":" ~~> (constantIndexGrammar *).inParenthesis
    val classIndexGrammar: BiGrammar = "class" ~~> constantIndexGrammar
    val parseIndexGrammar: BiGrammar = "extends" ~~> constantIndexGrammar
    val attributesGrammar = create(AttributesGrammar, attributeGrammar.manyVertical)
    val membersGrammar = create(MembersGrammar, print(Empty))
    val bodyGrammar = "{" % (membersGrammar % attributesGrammar.as(ClassAttributes)).indent() % "}"
    val classGrammar = create(Shape,
      (classIndexGrammar.as(ClassNameIndexKey) ~~ parseIndexGrammar.as(ClassParentIndex) ~~ interfacesGrammar.as(ClassInterfaces) %
        constantPool % bodyGrammar).asNode(Shape))

    find(BodyGrammar).inner = classGrammar
  }

  object ConstantPoolGrammar extends GrammarKey

  def getConstantPoolGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val constantPoolItemContent = create(ConstantPoolItemContentGrammar)
    val entries = constantPoolItemContent.manyVertical.indent()
    val result = "ConstantPool" ~ ":" %> entries.map[Seq[Any], ConstantPool] (
      entries => new ConstantPool(entries),
      constantPool => constantPool.constants.toSeq)
    create(ConstantPoolGrammar, result.as(ClassConstantPool))
  }

  override def description: String = "Defines a skeleton for bytecode."
}




