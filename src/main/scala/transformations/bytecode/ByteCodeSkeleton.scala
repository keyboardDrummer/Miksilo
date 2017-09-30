package transformations.bytecode

import core.bigrammar.BiGrammar
import core.document.Empty
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node._
import transformations.bytecode.ByteCodeMethodInfo.ByteCodeMethodInfoWrapper
import transformations.bytecode.attributes.{AttributeNameKey, ByteCodeAttribute}
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.javac.classes.ConstantPool

object ByteCodeSkeleton extends DeltaWithGrammar with WithState {

  implicit class ByteCodeWrapper[T <: NodeLike](node: T) {
    def constantPool: ConstantPool = node(ClassConstantPool).asInstanceOf[ConstantPool]
    def constantPool_=(constantPool: ConstantPool) = node(ClassConstantPool) = constantPool

    def methods: Seq[ByteCodeMethodInfoWrapper[T]] = node(ClassMethodsKey).asInstanceOf[Seq[Node]]
  }
  
  def getMethods[T <: NodeLike](clazz: T) = clazz(ClassMethodsKey).asInstanceOf[Seq[T]]

  def constantPoolGet(constantPool: ConstantPool, index: Int) = constantPool.getValue(index)

  def getAttributeNameIndex(attribute: Node) = attribute(AttributeNameKey).asInstanceOf[Int]

  def createState = new State()

  def clazz(name: Int, parent: Int, constantPool: ConstantPool, methods: Seq[Node], interfaces: Seq[Int] = Seq(),
            classFields: Seq[Node] = Seq(), attributes: Seq[Node] = Seq()) = new Node(ClassFileKey,
    ClassMethodsKey ->  methods,
    ClassNameIndexKey ->  name,
    ClassParentIndex ->  parent,
    ClassConstantPool ->  constantPool,
    ClassInterfaces ->  interfaces,
    ClassFields ->  classFields,
    ClassAttributes ->  attributes
  )

  def getParentIndex(clazz: Node) = clazz(ClassParentIndex).asInstanceOf[Int]

  def getClassNameIndex(clazz: Node) = clazz(ClassNameIndexKey).asInstanceOf[Int]

  def getClassInterfaces(clazz: Node) = clazz(ClassInterfaces).asInstanceOf[Seq[Int]]

  def getClassFields(clazz: Node) = clazz(ClassFields).asInstanceOf[Seq[Node]]

  def getClassAttributes(clazz: Node) = clazz(ClassAttributes).asInstanceOf[Seq[Node]]

  override def dependencies: Set[Contract] = Set.empty

  class State {
    val getBytes = new ClassRegistry[Node => Seq[Byte]]
    val attributes = new ClassRegistry[ByteCodeAttribute]
    val constantReferences = new ClassRegistry[Map[NodeField, NodeClass]]
  }


  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(ClassFileKey, Map(
      //TODO add with seq support //ClassInterfaces -> ClassRefConstant.key,
      ClassParentIndex -> ClassInfoConstant.key,
      ClassNameIndexKey -> ClassInfoConstant.key))
  }

  object ClassFileKey extends NodeClass

  object ClassMethodsKey extends NodeField

  object ClassNameIndexKey extends NodeField

  object ClassParentIndex extends NodeField

  object ClassConstantPool extends NodeField

  object ClassInterfaces extends NodeField

  object ClassFields extends NodeField

  object ClassAttributes extends NodeField

  object ConstantPoolItemContentGrammar

  object AttributeGrammar
  object MembersGrammar
  object AttributesGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val constantIndexGrammar = grammars.create(ConstantPoolIndexGrammar, integer)
    val program = grammars.find(ProgramGrammar)
    val attributeGrammar: BiGrammar = grammars.create(AttributeGrammar)
    val constantPool: BiGrammar = getConstantPoolGrammar(grammars)
    val interfacesGrammar: BiGrammar = "with interfaces:" ~~> (constantIndexGrammar *).inParenthesis
    val classIndexGrammar: BiGrammar = "class" ~~> constantIndexGrammar
    val parseIndexGrammar: BiGrammar = "extends" ~~> constantIndexGrammar
    val attributesGrammar = grammars.create(AttributesGrammar, attributeGrammar.manyVertical)
    val membersGrammar = grammars.create(MembersGrammar, print(Empty))
    val bodyGrammar = "{" % (membersGrammar % attributesGrammar.as(ClassAttributes)).indent() % "}"
    val classGrammar = grammars.create(ClassFileKey,
      (classIndexGrammar.as(ClassNameIndexKey) ~~ parseIndexGrammar.as(ClassParentIndex) ~~ interfacesGrammar.as(ClassInterfaces) %
        constantPool.as(ClassConstantPool) % bodyGrammar).asNode(ClassFileKey))

    program.inner = classGrammar
  }

  object ConstantPoolGrammar extends Key

  def getConstantPoolGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val constantPoolItemContent = grammars.create(ConstantPoolItemContentGrammar)
    val entries = constantPoolItemContent.manyVertical.indent()
    val result = "Constant pool:" %> entries ^^ (
      entries => new ConstantPool(entries.asInstanceOf[Seq[Any]]),
      constantPool => Some(constantPool.asInstanceOf[ConstantPool].constants.toSeq))
    grammars.create(ConstantPoolGrammar, result)
  }

  override def description: String = "Defines a skeleton for bytecode."
}




