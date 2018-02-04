package deltas.javac.classes.skeleton

import core.bigrammar.BiGrammar
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node._
import core.deltas.path.{NodePath, NodePathRoot}
import core.document.BlankLine
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.objects.{Declaration, NamedDeclaration}
import core.nabl.scopes.DeclarationInsideScope
import core.nabl.scopes.imports.DeclarationOfScope
import core.nabl.scopes.objects.Scope
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.bytecode.types.{ArrayTypeDelta, ObjectTypeDelta}
import deltas.javac.JavaLang
import deltas.javac.classes.ClassCompiler
import deltas.javac.statements.BlockDelta

object JavaClassSkeleton extends DeltaWithGrammar with DeltaWithPhase
  with WithLanguageRegistry with WithCompilationState {

  override def description: String = "Defines a skeleton for the Java class."

  implicit class JavaClass[T <: NodeLike](val node: T) extends AnyVal {
    def _package = node(ClassPackage).asInstanceOf[Seq[String]]
    def _package_=(value: Seq[String]) = node(ClassPackage) = value

    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]) = node(ClassImports) = value

    def name: String = node.getValue(ClassName)
    def name_=(value: String): Unit = node.setValue(ClassName, value)

    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]) = node(Members) = value

    def parent: Option[String] = node.getValue(ClassParent)
    def parent_=(value: Option[String]): Unit = node(ClassParent) = value
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    transformClass(program)

    def transformClass(javaClass: Node) {
      JavaLang.loadIntoClassPath(compilation)
      javaClass.shape = Shape
      val classCompiler: ClassCompiler = ClassCompiler(javaClass, compilation)
      getState(compilation).classCompiler = classCompiler
      classCompiler.bind()

      val classInfo = classCompiler.currentClassInfo
      javaClass(ByteCodeSkeleton.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      javaClass(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = javaClass.parent.get
      val parentRef = ClassInfoConstant.classRef(classCompiler.fullyQualify(parentName))
      javaClass(ByteCodeSkeleton.ClassParentIndex) = parentRef
      javaClass(ByteCodeSkeleton.ClassInterfaces) = Seq()

      for(member <- getRegistry(compilation).members)
        member.compile(compilation, javaClass)

      javaClass.data.remove(Members)
    }
  }

  def fullyQualify(_type: Node, classCompiler: ClassCompiler): Unit =  _type.shape match {
    case ArrayTypeDelta.ArrayTypeKey => fullyQualify(ArrayTypeDelta.getArrayElementType(_type), classCompiler)
    case ObjectTypeDelta.ObjectTypeKey =>
        val newName = ObjectTypeDelta.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeDelta.Name) = newName
    case _ =>
  }

  def getClassCompiler(compilation: Compilation): ClassCompiler = getState(compilation).classCompiler

  def getQualifiedClassName(javaClass: JavaClass[Node]): QualifiedClassName = {
    QualifiedClassName(javaClass._package ++ Seq(javaClass.name))
  }

  override def dependencies: Set[Contract] = Set(BlockDelta, InferredMaxStack, InferredStackFrames)

  object ClassMemberGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import language.grammars._

    val classMember: BiGrammar = create(ClassMemberGrammar)
    val importGrammar = create(ImportGrammar)
    val importsGrammar: BiGrammar = importGrammar.manyVertical as ClassImports
    val packageGrammar = (keyword("package") ~~> identifier.someSeparated(".") ~< ";") | value(Seq.empty) as ClassPackage
    val classParentGrammar = ("extends" ~~> identifier).option
    val nameGrammar: BiGrammar = "class" ~~> identifier
    val membersGrammar = "{".%((classMember.manySeparatedVertical(BlankLine) as Members).indent(BlockDelta.indentAmount)) % "}"
    val nameAndParent: BiGrammar = nameGrammar.as(ClassName) ~~ classParentGrammar.as(ClassParent)
    val classGrammar = packageGrammar % importsGrammar % nameAndParent % membersGrammar asLabelledNode Shape
    find(BodyGrammar).inner = classGrammar
  }

  object ImportGrammar extends GrammarKey
  object Shape extends ShapeWithDeclaration {
    override def collectDeclarationConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                               path: NodePath, defaultPackageScope: Scope): Declaration = {
      val clazz: JavaClass[NodePath] = NodePathRoot(compilation.program)
      val fullPackage: String = clazz._package.toList.fold("")((a, b) => a + "." + b)
      val packageDeclaration = builder.declaration(fullPackage, clazz.node, defaultPackageScope) //TODO because this is declared every time the package is referenced, there might be an issue.
      val packageScope = builder.declareScope(packageDeclaration)

      val clazzDeclaration = new NamedDeclaration(clazz.name, clazz.node)
      val classExternalScope = builder.newScope()
      builder.add(DeclarationInsideScope(clazzDeclaration, classExternalScope))
      builder.add(DeclarationOfScope(clazzDeclaration, classExternalScope))
      builder.importScope(packageScope, classExternalScope)

      val classInternalScope = builder.newScope()

      clazz.imports

      //TODO temp. replace with javaLang.
//      val objectDeclaration = builder.declaration("Object", clazz.node, classScope)
//      val objectScope = builder.declaredNewScope(objectDeclaration)
//      builder.declaration(SuperCallExpression.constructorName, clazz.node, objectScope, Some(ActionType(VoidTypeDelta.constraintType)))
//      //Exit temp.

      val members = clazz.members
      members.foreach(member => member.shape.asInstanceOf[ShapeWithDeclaration].
        collectDeclarationConstraints(compilation, builder, member, classInternalScope))

      clazzDeclaration
    }
  }

  def neww(_package: Seq[String], name: String, members: Seq[Node] = Seq(), imports: List[Node] = List(), mbParent: Option[String] = None) =
    new Node(Shape,
    Members -> members,
    ClassPackage -> _package,
    ClassName -> name,
    ClassImports -> imports,
    ClassParent -> mbParent)

  def createRegistry = new Registry()
  class Registry {
    var members = List.empty[ClassMemberDelta]
    val importToClassMap = new ShapeRegistry[(Compilation, Node) => Map[String, QualifiedClassName]]()
  }

  def createState = new State()
  class State {
    var classCompiler: ClassCompiler = _
    val javaCompiler: JavaCompiler = new JavaCompiler()
  }

  object ClassGrammar

  object ClassPackage extends NodeField

  object ClassImports extends NodeField

  object ClassParent extends NodeField

  object Members extends NodeField

  object ClassName extends NodeField

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation, builder) => {
      val defaultPackageScope = builder.newScope()
      val clazz: JavaClass[NodePath] = NodePathRoot(compilation.program)
      val clazzDeclaration = Shape.collectDeclarationConstraints(compilation, builder, clazz.node, defaultPackageScope)
      val classScope  = builder.resolveScopeDeclaration(clazzDeclaration)

      val proofs = JavaLang.getProofs(compilation)
      builder.proofs = proofs

      val members = clazz.members
      members.foreach(member =>
        member.shape.asInstanceOf[ShapeWithConstraints].collectConstraints(compilation, builder, member, classScope))
    }
    super.inject(language)
  }
}
