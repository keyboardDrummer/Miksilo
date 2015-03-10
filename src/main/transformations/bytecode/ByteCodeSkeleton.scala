package transformations.bytecode

import core.biGrammar.BiGrammar
import core.document.Empty
import core.grammar.StringLiteral
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import transformations.bytecode.attributes.Instruction
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.{ConstantPool, QualifiedClassName}


object ByteCodeSkeleton extends ParticleWithGrammar with Instruction with ParticleWithState {

  def getMethods(clazz: MetaObject) = clazz(ClassMethodsKey).asInstanceOf[Seq[MetaObject]]

  def constantPoolGet(constantPool: ConstantPool, index: Int) = constantPool.getValue(index)

  def getAttributeNameIndex(attribute: MetaObject) = attribute(AttributeNameKey).asInstanceOf[Int]

  def createState = new State()

  def clazz(name: Int, parent: Int, constantPool: ConstantPool, methods: Seq[MetaObject], interfaces: Seq[Int] = Seq(),
            classFields: Seq[MetaObject] = Seq(), attributes: Seq[MetaObject] = Seq()) = new MetaObject(ClassFileKey) {
    data.put(ClassMethodsKey, methods)
    data.put(ClassNameIndexKey, name)
    data.put(ClassParentIndex, parent)
    data.put(ClassConstantPool, constantPool)
    data.put(ClassInterfaces, interfaces)
    data.put(ClassFields, classFields)
    data.put(ClassAttributes, attributes)
  }

  def getParentIndex(clazz: MetaObject) = clazz(ClassParentIndex).asInstanceOf[Int]

  def getConstantPool(clazz: MetaObject): ConstantPool = clazz(ClassConstantPool).asInstanceOf[ConstantPool]

  def getClassNameIndex(clazz: MetaObject) = clazz(ClassNameIndexKey).asInstanceOf[Int]

  def getClassInterfaces(clazz: MetaObject) = clazz(ClassInterfaces).asInstanceOf[Seq[Int]]

  def getClassFields(clazz: MetaObject) = clazz(ClassFields).asInstanceOf[Seq[MetaObject]]

  def getClassAttributes(clazz: MetaObject) = clazz(ClassAttributes).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = Set.empty

  def getConstantPool(state: CompilationState) = getState(state).constantPool

  class State {
    var constantPool: ConstantPool = null
    val getBytes = new ClassRegistry[MetaObject => Seq[Byte]]
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
    val constantPoolItemContent = grammars.create(ConstantPoolItemContentGrammar,
        utf8 | qualifiedClassName)
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
    val removeIndexForParsing: (Any) => Seq[Any] = items => items.asInstanceOf[Seq[MetaObject]].map(i => i(ClassConstantEntryContent))
    val addIndexForPrinting: (Any) => Some[Seq[MetaObject]] = items => Some(items.asInstanceOf[Seq[Any]].zipWithIndex.map(p => new MetaObject(EnrichedClassConstantEntry,
      ClassConstantEntryIndex -> (p._2.asInstanceOf[Int] + 1),
      ClassConstantEntryContent -> p._1)))
    ( removeIndexForParsing, addIndexForPrinting )
  }

  override def description: String = "Defines a skeleton for bytecode."
}




