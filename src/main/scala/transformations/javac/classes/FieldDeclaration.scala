package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract, DeltaWithGrammar}
import transformations.bytecode.constants.FieldDescriptorConstant
import transformations.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton}
import transformations.bytecode.types.TypeSkeleton
import transformations.javac.classes.skeleton.{ClassMemberC, ClassSignature, JavaClassSkeleton}
import transformations.javac.classes.skeleton.JavaClassSkeleton._

object FieldDeclaration extends DeltaWithGrammar with ClassMemberC {

  object FieldKey extends Key
  object FieldType extends Key
  object FieldName extends Key

  override def dependencies: Set[Contract] = super.dependencies ++ Set(JavaClassSkeleton, FieldDescriptorConstant)

  def field(_type: Node, name: String) = new Node(FieldKey, FieldType -> _type, FieldName -> name)
  
    def bind(state: CompilationState, signature: ClassSignature, clazz: Node): Unit = {

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

  def compile(state: CompilationState, clazz: Node) = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)

    val fields = getFields(clazz)
    clazz(ByteCodeSkeleton.ClassFields) = fields.map(field => {
      convertField(field, classCompiler, state)
      field
    })
  }
  
  def convertField(field: Node, classCompiler: ClassCompiler, state: CompilationState) {
    val constantPool = classCompiler.constantPool
    val nameIndex = classCompiler.getNameIndex(getFieldName(field))

    field(ByteCodeFieldInfo.NameIndex) = nameIndex
    field.clazz = ByteCodeFieldInfo.FieldKey

    val fieldDescriptorIndex = constantPool.store(FieldDescriptorConstant.constructor(getFieldType(field)))
    field(ByteCodeFieldInfo.DescriptorIndex) = fieldDescriptorIndex
    field(ByteCodeFieldInfo.AccessFlagsKey) = Set.empty
    field(ByteCodeFieldInfo.FieldAttributes) = Seq.empty

    field.data.remove(FieldName)
    field.data.remove(FieldType)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val memberGrammar = grammars.find(JavaClassSkeleton.ClassMemberGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)

    val fieldGrammar = (typeGrammar ~~ identifier <~ ";").asNode(FieldKey, FieldType, FieldName)
    memberGrammar.addOption(fieldGrammar)
  }

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."
}
