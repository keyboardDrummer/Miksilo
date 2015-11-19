package transformations.bytecode

import core.bigrammar.BiGrammar
import core.document.Empty
import core.grammar.StringLiteral
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Node
import transformations.bytecode.attributes.ByteCodeAttribute
import transformations.javac.classes.ConstantPool
import transformations.javac.classes.skeleton.QualifiedClassName

object ByteCodeSkeleton extends ParticleWithGrammar with WithState {

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
  }

  object AttributeKey

  object AttributeNameKey

  object ClassFileKey

  object ClassMethodsKey

  object ClassNameIndexKey

  object ClassParentIndex

  object ClassConstantPool

  object ClassInterfaces

  object ClassFields

  object ClassAttributes

  private object EnrichedClassConstantEntry

  private object ClassConstantEntryIndex

  private object ClassConstantEntryContent

  object ConstantPoolItemContentGrammar

  object AttributeGrammar
  object MembersGrammar
  object AttributesGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val program = grammars.find(ProgramGrammar)
    val attributeGrammar: BiGrammar = grammars.create(AttributeGrammar)
    val constantPool: BiGrammar = getConstantPoolGrammar(grammars)
    val interfacesGrammar: BiGrammar = "with interfaces:" ~~> (number *).inParenthesis
    val classIndexGrammar: BiGrammar = "class" ~~> integer
    val parseIndexGrammar: BiGrammar = "extends" ~~> integer
    val attributesGrammar = grammars.create(AttributesGrammar, "attributes:" %> attributeGrammar.manyVertical.indent())
    val membersGrammar = grammars.create(MembersGrammar, Empty)
    val classGrammar = grammars.create(ClassFileKey, classIndexGrammar ~~ parseIndexGrammar ~~ interfacesGrammar %%
      constantPool %% membersGrammar %% attributesGrammar ^^
      parseMap(ClassFileKey, ClassNameIndexKey, ClassParentIndex, ClassInterfaces, ClassConstantPool,
        PartialSelf, ClassAttributes))

    program.inner = classGrammar
  }

  object ConstantPoolGrammar

  def getConstantPoolGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val utf8 = StringLiteral ^^ parseMapPrimitive(classOf[String])
    val qualifiedClassName: BiGrammar = getQualifiedClassNameParser
    val constantPoolItemContent = grammars.create(ConstantPoolItemContentGrammar, utf8 | qualifiedClassName)
    val constantPoolItem = ("#" ~> number <~ ":") ~~ constantPoolItemContent ^^
      parseMap(EnrichedClassConstantEntry, ClassConstantEntryIndex, ClassConstantEntryContent)
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




