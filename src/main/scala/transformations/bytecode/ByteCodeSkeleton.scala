package transformations.bytecode

import core.bigrammar.BiGrammar
import core.document.Empty
import core.grammar.StringLiteral
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.attributes.{AttributeNameKey, ByteCodeAttribute}
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.javac.classes.ConstantPool
import transformations.javac.classes.skeleton.QualifiedClassName

object ByteCodeSkeleton extends DeltaWithGrammar with WithState {

  implicit class ByteCode(node: Node) {
    def constantPool: ConstantPool = node(ClassConstantPool).asInstanceOf[ConstantPool]
    def constantPool_=(constantPool: ConstantPool) = node(ClassConstantPool) = constantPool
  }
  
  def getMethods(clazz: Node) = clazz(ClassMethodsKey).asInstanceOf[Seq[Node]]

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


  override def inject(state: CompilationState): Unit = {
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

  private object EnrichedClassConstantEntry extends Key

  private object ClassConstantEntryIndex extends Key

  private object ClassConstantEntryContent extends Key

  object ConstantPoolItemContentGrammar

  object AttributeGrammar
  object MembersGrammar
  object AttributesGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val constantIndexGrammar = grammars.create(ConstantPoolIndexGrammar, integer)
    val program = grammars.find(ProgramGrammar)
    val attributeGrammar: BiGrammar = grammars.create(AttributeGrammar)
    val constantPool: BiGrammar = getConstantPoolGrammar(grammars)
    val interfacesGrammar: BiGrammar = "with interfaces:" ~~> (constantIndexGrammar *).inParenthesis
    val classIndexGrammar: BiGrammar = "class" ~~> constantIndexGrammar
    val parseIndexGrammar: BiGrammar = "extends" ~~> constantIndexGrammar
    val attributesGrammar = grammars.create(AttributesGrammar, "attributes:" %> attributeGrammar.manyVertical.indent())
    val membersGrammar = grammars.create(MembersGrammar, print(Empty))
    val classGrammar = grammars.create(ClassFileKey,
      (classIndexGrammar.as(ClassNameIndexKey) ~~ parseIndexGrammar.as(ClassParentIndex) ~~ interfacesGrammar.as(ClassInterfaces) %%
        constantPool.as(ClassConstantPool) %% membersGrammar %% attributesGrammar.as(ClassAttributes)).asNode(ClassFileKey))

    program.inner = classGrammar
  }

  object ConstantPoolGrammar

  def getConstantPoolGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val utf8 = StringLiteral ^^ parseMapPrimitive(classOf[String])
    val qualifiedClassName: BiGrammar = getQualifiedClassNameParser
    val constantPoolItemContent = grammars.create(ConstantPoolItemContentGrammar, utf8 | qualifiedClassName)
    val constantPoolItem = (("#" ~> number <~ ":") ~~ constantPoolItemContent).
      asNode(EnrichedClassConstantEntry, ClassConstantEntryIndex, ClassConstantEntryContent)
    val entries = constantPoolItem.manyVertical.indent() ^^ biMapClassConstantEntryEnrichment
    val result = "constant pool:" %> entries ^^ (
      entries => new ConstantPool(entries.asInstanceOf[Seq[Any]]),
      constantPool => Some(constantPool.asInstanceOf[ConstantPool].constants.toSeq))
    grammars.create(ConstantPoolGrammar, result)
  }

  def getQualifiedClassNameParser: BiGrammar = {
    val construct: Any => Any = {
      case ids: Seq[Any] =>
        val stringIds = ids.collect({ case v: String => v})
        new QualifiedClassName(stringIds)
    }
    val parseQualifiedClassName = identifier.someSeparated(".") ^^(construct, {
      case QualifiedClassName(stringIds) => Some(stringIds)
      case _ => None
    })
    parseQualifiedClassName
  }

  def biMapClassConstantEntryEnrichment = {
    val removeIndexForParsing: (Any) => Seq[Any] = items => items.asInstanceOf[Seq[Node]].map(i => i(ClassConstantEntryContent))
    val addIndexForPrinting: (Any) => Some[Seq[Node]] = items => Some(items.asInstanceOf[Seq[Any]].zipWithIndex.map(p => new Node(EnrichedClassConstantEntry,
      ClassConstantEntryIndex -> (p._2.asInstanceOf[Int] + 1),
      ClassConstantEntryContent -> p._1)))
    ( removeIndexForParsing, addIndexForPrinting )
  }

  override def description: String = "Defines a skeleton for bytecode."
}




