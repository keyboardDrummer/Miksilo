package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.bytecode.extraConstants.TypeConstant
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton}
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.classes.skeleton.{ClassMemberC, ClassSignature, JavaClassSkeleton}

object FieldDeclaration extends DeltaWithGrammar with ClassMemberC {

  object FieldKey extends NodeClass
  object FieldType extends NodeField
  object FieldName extends NodeField

  override def dependencies: Set[Contract] = super.dependencies ++ Set(JavaClassSkeleton, TypeConstant)

  def field(_type: Node, name: String) = new Node(FieldKey, FieldType -> _type, FieldName -> name)
  
    def bind(state: Language, signature: ClassSignature, clazz: Node): Unit = {

    val fields = getFields(clazz)
    for (field <- fields)
      bindField(field)

    def bindField(field: Node) = {
      val name: String = getFieldName(field)
      val _type = getFieldType(field)
      signature.newFieldInfo(name, _type)
    }
  }

  def getFieldType(field: Node): Node = {
    field(FieldType).asInstanceOf[Node]
  }

  def getFieldName(field: Node): String = {
    field(FieldName).asInstanceOf[String]
  }

  def getFields(clazz: Node): Seq[Node] = {
    clazz.members.filter(member => member.clazz == FieldKey)
  }

  def compile(state: Language, clazz: Node) = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)

    val fields = getFields(clazz)
    clazz(ByteCodeSkeleton.ClassFields) = fields.map(field => {
      convertField(field, classCompiler, state)
      field
    })
  }
  
  def convertField(field: Node, classCompiler: ClassCompiler, state: Language) {
    val nameIndex = classCompiler.getNameIndex(getFieldName(field))

    field(ByteCodeFieldInfo.NameIndex) = nameIndex
    field.clazz = ByteCodeFieldInfo.FieldKey

    val fieldDescriptor = TypeConstant.constructor(getFieldType(field))
    field(ByteCodeFieldInfo.DescriptorIndex) = fieldDescriptor
    field(ByteCodeFieldInfo.AccessFlagsKey) = Set.empty
    field(ByteCodeFieldInfo.FieldAttributes) = Seq.empty

    field.data.remove(FieldName)
    field.data.remove(FieldType)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val memberGrammar = grammars.find(JavaClassSkeleton.ClassMemberGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)

    val fieldGrammar = (typeGrammar ~~ identifier <~ ";").asNode(FieldKey, FieldType, FieldName)
    memberGrammar.addOption(fieldGrammar)
  }

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."
}
