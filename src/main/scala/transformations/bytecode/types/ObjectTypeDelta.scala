package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword, Labelled}
import core.particles.Language
import core.particles.grammars.LanguageGrammars
import core.particles.node.{GrammarKey, Node, NodeClass, NodeField}
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.extraConstants.QualifiedClassNameConstantDelta
import transformations.javac.classes.skeleton.QualifiedClassName

object ObjectTypeDelta extends TypeInstance with StackType {
  override val key = ObjectTypeKey
  val stringType = objectType(new QualifiedClassName(Seq("java", "lang", "String")))
  val rootObjectType = objectType(new QualifiedClassName(Seq("java", "lang", "Object")))

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = {
    Seq.empty //TODO extend
  }

  def stackObjectType(constantPoolClassRef: Int) = new Node(ObjectTypeKey, Name -> constantPoolClassRef)

  object ObjectTypeJavaGrammar extends GrammarKey
  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val construct: Any => Any = {
      case ids: Seq[Any] =>
        val stringIds = ids.collect({ case v: String => v})
        if (stringIds.size > 1)
          Right(QualifiedClassName(stringIds))
        else
          Left(stringIds.last)
    }
    def deconstruct(value: Any): Option[Any] = Some(value match {
      case Right(QualifiedClassName(stringIds)) => stringIds
      case Left(string) => Seq(string)
    })
    val parseObjectType = create(ObjectTypeJavaGrammar,
      (identifier.someSeparated(".") ^^ (construct, deconstruct)).as(Name).asNode(ObjectTypeKey))
    parseObjectType
  }

  def objectType(name: QualifiedClassName) = new Node(ObjectTypeKey,
    Name -> Right(name))

  def objectType(className: String) = new Node(ObjectTypeKey,
    Name -> Left(className))

  object ObjectTypeByteCodeGrammar extends GrammarKey
  object ObjectTypeByteCodeGrammarInner extends GrammarKey
  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    val construct: Any => Any = {
      case name: QualifiedClassName =>
        Right(name)
    }
    def deconstruct(value: Any): Option[Any] = Some(value match {
      case Right(name) => name
    })

    val qualifiedClassNameParser = QualifiedClassNameConstantDelta.getQualifiedClassNameParser(grammars)
    import grammars._
    val inner: Labelled = create(ObjectTypeByteCodeGrammarInner,
      (qualifiedClassNameParser ^^ (construct, deconstruct)).as(Name).asNode(ObjectTypeKey))
    val grammar: BiGrammar = Keyword("L", reserved = false) ~> inner ~< ";"
    create(ObjectTypeByteCodeGrammar, grammar)
  }

  def getObjectTypeName(objectType: Node): Either[String, QualifiedClassName] = objectType(Name).asInstanceOf[Either[String, QualifiedClassName]]

  override def getStackSize: Int = 1

  object Name  extends NodeField

  object ObjectTypeKey extends NodeClass

  object ObjectStackType extends NodeClass

  override def getStackType(_type: Node, state: Language): Node = {
    ObjectStackType.create(Name -> ClassInfoConstant.classRef(ObjectTypeDelta.getObjectTypeName(_type).right.get))
  }

  override def description: String = "Defines the object type."
}
