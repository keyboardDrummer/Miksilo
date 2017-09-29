package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword, Labelled}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.extraConstants.QualifiedClassNameConstant
import transformations.javac.classes.skeleton.QualifiedClassName

object ObjectTypeDelta extends TypeInstance with StackType {
  override val key = ObjectTypeKey
  val stringType = objectType(new QualifiedClassName(Seq("java", "lang", "String")))
  val rootObjectType = objectType(new QualifiedClassName(Seq("java", "lang", "Object")))

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = {
    Seq.empty //TODO extend
  }

  def stackObjectType(constantPoolClassRef: Int) = new Node(ObjectTypeKey, Name -> constantPoolClassRef)

  object ObjectTypeJavaGrammar
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
    val parseObjectType = grammars.create(ObjectTypeJavaGrammar,
      (identifier.someSeparated(".") ^^ (construct, deconstruct)).asNode(ObjectTypeKey, Name))
    parseObjectType
  }

  def objectType(name: QualifiedClassName) = new Node(ObjectTypeKey,
    Name -> Right(name))

  def objectType(className: String) = new Node(ObjectTypeKey,
    Name -> Left(className))

  object ObjectTypeByteCodeGrammar
  object ObjectTypeByteCodeGrammarInner
  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val construct: Any => Any = {
      case name: QualifiedClassName =>
        Right(name)
    }
    def deconstruct(value: Any): Option[Any] = Some(value match {
      case Right(name) => name
    })
    val inner: Labelled = grammars.create(ObjectTypeByteCodeGrammarInner,
      (QualifiedClassNameConstant.getQualifiedClassNameParser ^^ (construct, deconstruct)).asNode(ObjectTypeKey, Name))
    val grammar: BiGrammar = Keyword("L", false) ~> inner <~ ";"
    grammars.create(ObjectTypeByteCodeGrammar, grammar)
  }

  def getObjectTypeName(objectType: Node): Either[String, QualifiedClassName] = objectType(Name).asInstanceOf[Either[String, QualifiedClassName]]

  override def getStackSize: Int = 1

  object Name  extends NodeField

  object ObjectTypeKey extends NodeClass

  override def getStackType(_type: Node, state: Language): Node = {
    ObjectTypeKey.create(Name -> ClassInfoConstant.classRef(ObjectTypeDelta.getObjectTypeName(_type).right.get))
  }

  override def description: String = "Defines the object type."
}
