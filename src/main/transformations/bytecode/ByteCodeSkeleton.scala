package transformations.bytecode

import core.grammar.StringLiteral
import core.grammarDocument.{BiGrammar, ManyVertical}
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.attributes.{Instruction, SourceFileAttribute}
import transformations.bytecode.constants._
import transformations.javac.classes.{ConstantPool, QualifiedClassName}
import transformations.types._

import scala.collection.mutable

object ByteCodeSkeleton extends GrammarTransformation
  with NameAndType with MethodRefConstant with FieldRefConstant with MethodDescriptorConstant with ClassRefConstant with SourceFileAttribute
  with MethodInfo with Instruction {

  def getInstructionSizeRegistry(state: TransformationState) = getState(state).getInstructionSizeRegistry

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  def getInstructionSignatureRegistry(state: TransformationState) = getState(state).getInstructionSignatureRegistry

  def getInstructionStackSizeModificationRegistry(state: TransformationState) = getState(state).getInstructionStackSizeModificationRegistry

  def getMethodAttributes(method: MetaObject) = method(MethodAnnotations).asInstanceOf[Seq[MetaObject]]

  def getMethods(clazz: MetaObject) = clazz(ClassMethodsKey).asInstanceOf[Seq[MetaObject]]

  def constantPoolGet(constantPool: ConstantPool, index: Int) = constantPool.getValue(index)

  def getAttributeNameIndex(attribute: MetaObject) = attribute(AttributeNameKey).asInstanceOf[Int]

  def clazz(name: Int, parent: Int, constantPool: mutable.Buffer[Any], methods: Seq[MetaObject], interfaces: Seq[Int] = Seq(),
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

  def getConstantPool(clazz: MetaObject) = clazz(ClassConstantPool).asInstanceOf[mutable.Buffer[Any]]

  def getClassNameIndex(clazz: MetaObject) = clazz(ClassNameIndexKey).asInstanceOf[Int]

  def getClassInterfaces(clazz: MetaObject) = clazz(ClassInterfaces).asInstanceOf[Seq[Int]]

  def getClassFields(clazz: MetaObject) = clazz(ClassFields).asInstanceOf[Seq[MetaObject]]

  def getClassAttributes(clazz: MetaObject) = clazz(ClassAttributes).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = Set(ObjectTypeC, DoubleTypeC, ArrayTypeC, BooleanTypeC, LongTypeC, VoidTypeC)

  case class JumpBehavior(movesToNext: Boolean, hasJumpInFirstArgument: Boolean)

  def getConstantPool(state: TransformationState) = getState(state).constantPool

  class State {
    var constantPool: ConstantPool = null
    val getInstructionStackSizeModificationRegistry = new mutable.HashMap[Any, (ConstantPool, MetaObject) => Int]
    val getInstructionSignatureRegistry = new mutable.HashMap[Any, (ConstantPool, MetaObject) => (Seq[MetaObject], Seq[MetaObject])]
    val getInstructionSizeRegistry = new mutable.HashMap[Any, MetaObject => Int]
    val jumpBehaviorRegistry = new mutable.HashMap[Any, JumpBehavior]
    val localUpdates = new mutable.HashMap[Any, MetaObject => Map[Int, MetaObject]]
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

  object MethodsGrammar
  object AttributeGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val program = grammars.find(ProgramGrammar)
    grammars.create(AttributeGrammar)
    val constantPool: BiGrammar = getConstantPoolGrammar(grammars)
    val methodInfoGrammar: BiGrammar = getMethodInfoGrammar(grammars)
    val methods = grammars.create(MethodsGrammar, "Methods:" %> methodInfoGrammar.manyVertical.indent(2))
    val classGrammar = grammars.create(ClassFileKey, "class" ~~> number %% constantPool %% methods ^^
      parseMap(ClassFileKey, ClassNameIndexKey, ClassConstantPool, ClassMethodsKey))

    program.inner = classGrammar
  }

  object ConstantPoolGrammar

  def getConstantPoolGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val parseType: BiGrammar = grammars.find(TypeC.TypeGrammar)
    val utf8 = StringLiteral ^^ parseMapPrimitive(classOf[String])
    val qualifiedClassName: BiGrammar = getQualifiedClassNameParser
    val objectTypeGrammar = grammars.find(ObjectTypeC.ObjectTypeGrammar) // TODO object type shouldn't be in the constantPool.
    val constantPoolItemContent = grammars.create(ConstantPoolItemContentGrammar,
        utf8 | qualifiedClassName | classRefGrammar | getMethodDescriptorGrammar(parseType) | nameAndTypeGrammar |
          methodRefGrammar | objectTypeGrammar | fieldRefGrammar)
    val constantPoolItem = ("#" ~> number <~ ":") ~~ constantPoolItemContent ^^
      parseMap(EnrichedClassConstantEntry, ClassConstantEntryIndex, ClassConstantEntryContent)
    val result = "ConstantPool:" %> constantPoolItem.manyVertical.indent() ^^ biMapClassConstantEntryEnrichment
    grammars.create(ConstantPoolGrammar, result)
  }

  object AccessFlagGrammar
  object MethodInfoGrammar
  def getMethodInfoGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val parseAttribute = grammars.find(AttributeGrammar)
    val parseAccessFlag = grammars.create(AccessFlagGrammar, "ACC_PUBLIC" ~> produce(PublicAccess) | "ACC_STATIC" ~> produce(StaticAccess) | "ACC_PRIVATE" ~> produce(PrivateAccess))
    val methodHeader: BiGrammar = Seq[BiGrammar](
      "nameIndex:" ~> number,
      "descriptorIndex:" ~> number,
      "flags:" ~> parseAccessFlag.manySeparated(", ").seqToSet).
      reduce((l, r) => (l <~ ",") ~~ r)
    val methodInfoGrammar: BiGrammar = methodHeader % ("attributes:" %> new ManyVertical(parseAttribute).indent(2)) ^^
      parseMap(MethodInfoKey, MethodNameIndex, MethodDescriptorIndex, MethodAccessFlags, MethodAnnotations)
    grammars.create(MethodInfoGrammar, methodInfoGrammar)
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
}




