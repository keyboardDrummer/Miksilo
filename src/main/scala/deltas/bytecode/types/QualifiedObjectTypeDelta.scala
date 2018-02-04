package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.{Keyword, Labelled}
import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.{Type, TypeFromDeclaration}
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.javac.classes.skeleton.QualifiedClassName

object QualifiedObjectTypeDelta extends TypeInstance with StackType {

  override val key: Shape.type = Shape

  override def description: String = "Defines the object type based on a fully qualified class name."

  val stringType: Node = neww(QualifiedClassName(Seq("java", "lang", "String")))
  val rootObjectType: Node = neww(QualifiedClassName(Seq("java", "lang", "Object")))

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = {
    Seq.empty //TODO extend
  }

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    identifier.someSeparated(".").as(Name).asLabelledNode(Shape)
  }

  def neww(name: QualifiedClassName) = new Node(Shape, Name -> name)

  object ByteCodeGrammarInner extends GrammarKey
  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    val qualifiedClassNameParser = QualifiedClassNameConstantDelta.getQualifiedClassNameParser(grammars)
    import grammars._
    val inner: Labelled = create(ByteCodeGrammarInner, qualifiedClassNameParser.as(Name).asNode(Shape))
    Keyword("L", reserved = false) ~> inner ~< ";"
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
    val packageFull = packageParts.reduce((a,b) => a + "." + b)
    val packageDeclaration = builder.resolve2(packageFull, _type.asPath, parentScope)
    val packageScope = builder.resolveScopeDeclaration(packageDeclaration)
    val classDeclaration = builder.resolve2(className, _type.asPath, packageScope)
    TypeFromDeclaration(classDeclaration)
  }
}
