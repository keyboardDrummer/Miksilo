package transformations.bytecode

import core.document.Empty
import core.grammar.StringLiteral
import core.grammarDocument.GrammarDocument
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.CodeAnnotation.CodeAttributeId
import transformations.bytecode.StackMapTableC.StackMapTableId
import transformations.javac.classes.{ConstantPool, QualifiedClassName}
import transformations.types._

import scala.collection.mutable

object ByteCodeSkeleton extends GrammarTransformation {

  def getInstructionSizeRegistry(state: TransformationState) = getState(state).getInstructionSizeRegistry

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  def getInstructionSignatureRegistry(state: TransformationState) = getState(state).getInstructionSignatureRegistry

  def getInstructionStackSizeModificationRegistry(state: TransformationState) = getState(state).getInstructionStackSizeModificationRegistry

  def getMethodAttributes(method: MetaObject) = method(MethodAnnotations).asInstanceOf[Seq[MetaObject]]

  def getMethods(clazz: MetaObject) = clazz(ClassMethodsKey).asInstanceOf[Seq[MetaObject]]

  def sourceFile(nameIndex: Int, fileNameIndex: Int): MetaObject = new MetaObject(SourceFileAttribute) {
    data.put(SourceFileFileNameIndex, fileNameIndex)
    data.put(AttributeNameKey, nameIndex)
  }

  def getSourceFileFileNameIndex(sourceFile: MetaObject) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  def nameAndType(nameIndex: Int, typeIndex: Int): MetaObject = new MetaObject(NameAndTypeKey) {
    data.put(NameAndTypeName, nameIndex)
    data.put(NameAndTypeType, typeIndex)
  }

  def getNameAndTypeName(nameAndType: MetaObject) = nameAndType(NameAndTypeName).asInstanceOf[Int]

  def getNameAndTypeType(nameAndType: MetaObject) = nameAndType(NameAndTypeType).asInstanceOf[Int]

  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }

  def getClassRefName(classRef: MetaObject) = classRef(ClassRefName).asInstanceOf[Int]

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new MetaObject(MethodRefKey) {
    data.put(MethodRefClassName, classNameIndex)
    data.put(MethodRefMethodName, methodNameAndTypeIndex)
  }

  def getMethodRefClassRefIndex(methodRef: MetaObject) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getMethodRefMethodNameIndex(methodRef: MetaObject) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def instruction(_type: AnyRef, arguments: Seq[Any] = Seq()) = new MetaObject(_type) {
    data.put(InstructionArgumentsKey, arguments)
  }

  def methodInfo(nameIndex: Int, descriptorIndex: Int, annotations: Seq[MetaObject], flags: Set[MethodAccessFlag] = Set()) =
    new MetaObject(MethodInfoKey) {
      data.put(MethodAnnotations, annotations)
      data.put(MethodNameIndex, nameIndex)
      data.put(MethodDescriptorIndex, descriptorIndex)
      data.put(MethodAccessFlags, flags)
    }

  def getMethodAccessFlags(method: MetaObject) = method(MethodAccessFlags).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]

  def getInstructionArguments(instruction: MetaObject) = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Int]]

  def setInstructionArguments(instruction: MetaObject, arguments: Seq[Any]) {
    instruction(InstructionArgumentsKey) = arguments
  }

  def constantPoolGet(constantPool: ConstantPool, index: Int) = constantPool.getValue(index)

  def methodDescriptor(returnDescriptor: MetaObject, parameterDescriptors: Seq[MetaObject]) = {
    new MetaObject(MethodDescriptor) {
      data.put(MethodDescriptorParameters, parameterDescriptors)
      data.put(MethodReturnType, returnDescriptor)
    }
  }

  def getMethodDescriptorReturnType(descriptor: MetaObject) = descriptor(MethodReturnType).asInstanceOf[MetaObject]

  def getMethodDescriptorParameters(descriptor: MetaObject) = descriptor(MethodDescriptorParameters).asInstanceOf[Seq[MetaObject]]


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

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new MetaObject(FieldRef) {
    data.put(FieldRefClassIndex, classIndex)
    data.put(FieldRefNameAndTypeIndex, nameAndTypeIndex)
  }

  def getFieldRefClassIndex(fieldRef: MetaObject) = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getFieldRefNameAndTypeIndex(fieldRef: MetaObject) = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set(ObjectTypeC, DoubleTypeC, ArrayTypeC, BooleanTypeC, LongTypeC, VoidTypeC)

  trait MethodAccessFlag

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

  object SourceFileAttribute

  object SourceFileFileNameIndex

  object SourceFileId

  object NameAndTypeKey

  object NameAndTypeName

  object NameAndTypeType

  object ClassRefKey

  object ClassRefName

  object MethodRefKey

  object MethodRefClassName

  object MethodRefMethodName

  object MethodInfoKey

  object MethodNameIndex

  object MethodDescriptorIndex

  object MethodAnnotations

  object MethodAccessFlags

  object PublicAccess extends MethodAccessFlag

  object StaticAccess extends MethodAccessFlag

  object PrivateAccess extends MethodAccessFlag

  object InstructionArgumentsKey

  object MethodDescriptor

  object MethodDescriptorParameters

  object MethodReturnType

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

  object FieldRef

  object FieldRefClassIndex

  object FieldRefNameAndTypeIndex

  private object EnrichedClassConstantEntry

  private object ClassConstantEntryIndex

  private object ClassConstantEntryContent

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val program = grammars.find(ProgramGrammar)

    val codeAttribute = "Code" ~> produce(CodeAttributeId)
    val stackMapTableAttribute = "StackMapTable" ~> produce(StackMapTableId)
    val methodRef = "method reference:" ~~> (number <~ ".") ~ number ^^ parseMap(MethodRefKey, MethodRefClassName, MethodRefMethodName)
    val fieldRef = "field reference:" ~~> (number <~ ".") ~ number ^^ parseMap(FieldRef, FieldRefClassIndex, FieldRefNameAndTypeIndex)
    val classRefKey = "class reference:" ~~> number ^^ parseMap(ClassRefKey, ClassRefName)
    val nameAndType = "name and type: " ~~> (number <~ ":") ~ number ^^ parseMap(NameAndTypeKey, NameAndTypeName, NameAndTypeType)
    val parseType : GrammarDocument = grammars.find(TypeC.TypeGrammar)
    val methodDescriptor = (parseType ~~ parseType.manySeparated(";").inParenthesis) ^^
      parseMap(MethodDescriptor, MethodReturnType, MethodDescriptorParameters)
    val utf8 = StringLiteral ^^ parseMapPrimitive(classOf[String])
    val qualifiedClassName: GrammarDocument = getQualifiedClassNameParser
    val objectTypeGrammar = grammars.find(ObjectTypeC.ObjectTypeGrammar) // TODO object type shouldn't be in the constantPool.
    val constantPoolItemContent = utf8 | qualifiedClassName | classRefKey | methodDescriptor | nameAndType |
      methodRef | codeAttribute | objectTypeGrammar | fieldRef | stackMapTableAttribute
    val constantPoolItem = ("#" ~> number <~ ":") ~~ constantPoolItemContent ^^
      parseMap(EnrichedClassConstantEntry, ClassConstantEntryIndex, ClassConstantEntryContent)
    val constantPool = "ConstantPool:" %> constantPoolItem.manySeparatedVertical(Empty) ^^ biMapClassConstantEntryEnrichment
    val classGrammar = grammars.create(ClassFileKey, "class" ~~> identifier %% constantPool ^^
      parseMap(ClassFileKey, ClassNameIndexKey, ClassConstantPool))

    program.inner = classGrammar
  }

  def getQualifiedClassNameParser: GrammarDocument = {
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

  def biMapClassConstantEntryEnrichment: ((Any) => Seq[Any], (Any) => Some[Seq[MetaObject]]) = {
    ( {
      case items: Seq[MetaObject] => items.map(i => i(ClassConstantEntryContent))
    }, {
      case items: Seq[Any] => Some(items.zipWithIndex.map(p => new MetaObject(EnrichedClassConstantEntry,
        ClassConstantEntryIndex -> (p._2.asInstanceOf[Int] + 1),
        ClassConstantEntryContent -> p._1)))
    })
  }
}
