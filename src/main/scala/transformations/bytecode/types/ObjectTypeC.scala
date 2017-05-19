package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword, Labelled}
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.javac.classes.skeleton.QualifiedClassName

object ObjectTypeC extends TypeInstance with StackType {
  override val key = ObjectTypeKey
  val stringType = objectType(new QualifiedClassName(Seq("java", "lang", "String")))
  val rootObjectType = objectType(new QualifiedClassName(Seq("java", "lang", "Object")))

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = {
    Seq.empty //TODO extend
  }

  def stackObjectType(constantPoolClassRef: Int) = new Node(ObjectTypeKey, ObjectTypeName -> constantPoolClassRef)

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
      (identifier.someSeparated(".") ^^ (construct, deconstruct)).asNode(ObjectTypeKey, ObjectTypeName))
    parseObjectType
  }

  def objectType(name: QualifiedClassName) = new Node(ObjectTypeKey,
    ObjectTypeName -> Right(name))

  def objectType(className: String) = new Node(ObjectTypeKey,
    ObjectTypeName -> Left(className))

  object ObjectTypeByteCodeGrammar
  object ObjectTypeByteCodeGrammarInner
  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val construct: Any => Any = {
      case ids: Seq[Any] =>
        val stringIds = ids.collect({ case v: String => v})
        Right(new QualifiedClassName(stringIds))
    }
    def deconstruct(value: Any): Option[Any] = Some(value match {
      case Right(QualifiedClassName(stringIds)) => stringIds
    })
    val inner: Labelled = grammars.create(ObjectTypeByteCodeGrammarInner,
      (identifier.someSeparated("/") ^^ (construct, deconstruct)).asNode(ObjectTypeKey, ObjectTypeName))
    val grammar: BiGrammar = new Keyword("L",false) ~> inner <~ ";"
    grammars.create(ObjectTypeByteCodeGrammar, grammar)
  }

  def getObjectTypeName(objectType: Node): Either[String, QualifiedClassName] = objectType(ObjectTypeName).asInstanceOf[Either[String, QualifiedClassName]]

  override def getStackSize: Int = 1

  object ObjectTypeName  extends Key

  object ObjectTypeKey extends Key

  override def getStackType(_type: Node, state: CompilationState): Node = {
    ObjectTypeC.stackObjectType(state.program.constantPool.getClassRef(ObjectTypeC.getObjectTypeName(_type).right.get))
  }

  override def description: String = "Defines the object type."
}
