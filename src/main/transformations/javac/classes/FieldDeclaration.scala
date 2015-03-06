package transformations.javac.classes

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.constants.FieldDescriptorConstant
import transformations.bytecode.{ByteCodeField, ByteCodeSkeleton}
import transformations.types.TypeC

object FieldDeclaration extends GrammarTransformation {

  object FieldKey
  object FieldType
  object FieldName

  override def dependencies: Set[Contract] = super.dependencies ++ Set(ClassC, FieldDescriptorConstant)

  override def inject(state: TransformationState): Unit = {
    super.inject(state)

    ClassC.getState(state).firstMemberPasses ::= (clazz => bindFields(state, clazz))
    ClassC.getState(state).secondMemberPasses ::= (clazz => convertFields(state, clazz))
  }
  
  def bindFields(state: TransformationState, clazz: MetaObject): Unit = {
    val classCompiler = ClassC.getClassCompiler(state)
    val classInfo = classCompiler.currentClassInfo

    val fields = getFields(clazz)
    for (field <- fields)
      bindField(field)

    def bindField(field: MetaObject) = {
      val name: String = getFieldName(field)
      val _type = getFieldType(field)
      classInfo.newFieldInfo(name, _type)
    }
  }

  def getFieldType(field: MetaObject): MetaObject = {
    field(FieldType).asInstanceOf[MetaObject]
  }

  def getFieldName(field: MetaObject): String = {
    field(FieldName).asInstanceOf[String]
  }

  def getFields(clazz: MetaObject): Seq[MetaObject] = {
    ClassC.getMembers(clazz).filter(member => member.clazz == FieldKey)
  }

  def convertFields(state: TransformationState, clazz: MetaObject) = {
    val classCompiler = ClassC.getClassCompiler(state)

    val fields = getFields(clazz)
    clazz(ByteCodeSkeleton.ClassFields) = fields.map(field => {
      convertField(field, classCompiler, state)
      field
    })
  }
  
  def convertField(field: MetaObject, classCompiler: ClassCompiler, state: TransformationState) {
    val constantPool = classCompiler.constantPool
    val nameIndex = classCompiler.getNameIndex(getFieldName(field))

    field(ByteCodeField.NameIndex) = nameIndex

    val fieldDescriptorIndex = constantPool.store(FieldDescriptorConstant.constructor(getFieldType(field)))
    field(ByteCodeField.DescriptorIndex) = fieldDescriptorIndex

    field.data.remove(FieldName)
    field.data.remove(FieldType)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val memberGrammar = grammars.find(ClassC.ClassMemberGrammar)
    val typeGrammar = grammars.find(TypeC.TypeGrammar)

    val fieldGrammar = typeGrammar ~~ identifier <~ ";" ^^ parseMap(FieldKey, FieldType, FieldName)
    memberGrammar.addOption(fieldGrammar)
  }
}
