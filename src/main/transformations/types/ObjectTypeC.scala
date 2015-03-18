package transformations.types

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.QualifiedClassName

object ObjectTypeC extends TypeInstance {
  override val key: AnyRef = ObjectTypeKey
  val stringType = objectType(new QualifiedClassName(Seq("java", "lang", "String")))

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = {
    Seq.empty //TODO extend
  }

  def stackObjectType(constantPoolClassRef: Int) = new Node(ObjectTypeKey, ObjectTypeName -> constantPoolClassRef)

  object ObjectTypeGrammar
  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val construct: Any => Any = {
      case ids: Seq[Any] =>
        val stringIds = ids.collect({ case v: String => v})
        if (stringIds.size > 1)
          Right(new QualifiedClassName(stringIds))
        else
          Left(stringIds.last)
    }
    def deconstruct(value: Any): Option[Any] = Some(value match {
      case Right(QualifiedClassName(stringIds)) => stringIds
      case Left(string) => Seq(string)
    })
    val parseObjectType = grammars.create(ObjectTypeGrammar,
      identifier.someSeparated(".") ^^ (construct, deconstruct) ^^ parseMap(ObjectTypeKey, ObjectTypeName))
    parseObjectType
  }

  def objectType(name: QualifiedClassName) = new Node(ObjectTypeKey,
    ObjectTypeName -> Right(name))

  def objectType(className: String) = new Node(ObjectTypeKey,
    ObjectTypeName -> Left(className))

  override def getByteCodeString(_type: Node, state: CompilationState): String =
    s"L${getObjectTypeName(_type).right.get.parts.mkString("/")};"

  def getObjectTypeName(objectType: Node): Either[String, QualifiedClassName] = objectType(ObjectTypeName).asInstanceOf[Either[String, QualifiedClassName]]

  override def getStackSize: Int = 1

  object ObjectTypeName

  object ObjectTypeKey

  override def getStackType(_type: Node, state: CompilationState): Node = {
    ObjectTypeC.stackObjectType(ByteCodeSkeleton.getConstantPool(state).getClassRef(ObjectTypeC.getObjectTypeName(_type).right.get))
  }

  override def description: String = "Defines the object type."
}
