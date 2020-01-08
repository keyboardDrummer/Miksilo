package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Labelled
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.FieldPath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.NamedDeclaration
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{Type, TypeFromDeclaration}
import deltas.bytecode.constants.ClassInfoConstant
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass
import deltas.javac.classes.skeleton.QualifiedClassName

object QualifiedObjectTypeDelta extends ByteCodeTypeInstance with HasStackTypeDelta {

  override val shape: Shape.type = Shape

  override def description: String = "Defines the object type based on a fully qualified class name."

  val stringType: Node = neww(QualifiedClassName(Seq("java", "lang", "String")))
  val rootObjectType: Node = neww(QualifiedClassName(Seq("java", "lang", "Object")))

  override def getSuperTypes(_type: Node): Seq[Node] = {
    Seq.empty //TODO extend
  }

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val parseQualifiedClassName = (identifier ~< "." ~ identifier.someSeparated(".")).
      map[(String, Seq[String]), QualifiedClassName](
        p => QualifiedClassName(Seq(p._1) ++ p._2),
        qualifiedClassName => (qualifiedClassName.parts.head, qualifiedClassName.parts.tail))
    parseQualifiedClassName.as(Name).asLabelledNode(Shape)
  }

  def neww(name: QualifiedClassName) = new Node(Shape, Name -> name)

  object ByteCodeGrammarInner extends GrammarKey
  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    val qualifiedClassNameParser = getQualifiedClassNameParser(grammars)
    import grammars._
    val inner: Labelled = create(ByteCodeGrammarInner, qualifiedClassNameParser.as(Name).asNode(Shape))
    stringToGrammar("L", reserved = false) ~> inner ~< ";"
  }

  def getQualifiedClassNameParser(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    identifier.someSeparated("/").map[Seq[String], QualifiedClassName](QualifiedClassName, qualifiedClassName => qualifiedClassName.parts)
  }

  def getName(objectType: NodeLike): QualifiedClassName = objectType.getValue(Name).asInstanceOf[QualifiedClassName]

  override def getStackSize: Int = 1

  object Shape extends NodeShape
  object Name extends NodeField

  object StackType extends NodeShape //TODO make this a separate delta.
  override def getStackType(_type: Node, language: Language): Node = {
    StackType.create(Name -> ClassInfoConstant.classRef(getName(_type)))
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val name = getName(_type)
    val packageParts = name.parts.dropRight(1)
    val className = name.parts.last
    val scope = if (packageParts.nonEmpty) {
      val packageFull = packageParts.reduce((a,b) => a + "." + b)
      val packageDeclaration = builder.resolveOption(packageFull, origin = _type.asPath, parentScope)
      builder.getDeclaredScope(packageDeclaration)
    } else {
      parentScope
    }
    val classDeclaration = builder.resolveOption(className, origin = _type.asPath, scope)
    TypeFromDeclaration(classDeclaration)
  }

  override def constraintName = "DECLARATION"

  val _clazz = new TypedNodeField[Node]("clazz")
  override def fromConstraintType(_type: Type): Node = {
    val clazz = _type.asInstanceOf[TypeFromDeclaration].declaration.asInstanceOf[NamedDeclaration].origin.get.asInstanceOf[FieldPath].parent
    val result = neww(JavaClassDelta.getQualifiedClassName(clazz))
    _clazz(result) = clazz
    result
  }
}
