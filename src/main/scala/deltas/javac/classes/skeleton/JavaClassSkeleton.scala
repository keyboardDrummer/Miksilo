package deltas.javac.classes.skeleton

import core.bigrammar.BiGrammar
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node._
import core.deltas.path.{ChildPath, Path, PathRoot}
import core.document.BlankLine
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.objects.{Declaration, NamedDeclaration}
import core.smarts.scopes.DeclarationInsideScope
import core.smarts.scopes.imports.DeclarationOfScope
import core.smarts.scopes.objects.Scope
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, UnqualifiedObjectTypeDelta}
import deltas.javac.JavaLang
import deltas.javac.classes.ClassCompiler
import deltas.javac.statements.BlockDelta

import scala.collection.mutable

object JavaClassSkeleton extends DeltaWithGrammar with DeltaWithPhase
  with WithLanguageRegistry with WithCompilationState with HasDeclaration {

  override def description: String = "Defines a skeleton for the Java class."

  implicit class JavaClass[T <: NodeLike](val node: T) extends AnyVal {
    def _package: Seq[String] = node(ClassPackage).asInstanceOf[Seq[String]]
    def _package_=(value: Seq[String]) = node(ClassPackage) = value

    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]) = node(ClassImports) = value

    def name: String = node(ClassName).asInstanceOf[String]
    def name_=(value: String): Unit = node(ClassName) = value

    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]) = node(Members) = value

    def parent: Option[String] = node(ClassParent).asInstanceOf[Option[String]]
    def parent_=(value: Option[String]): Unit = node(ClassParent) = value
  }

  override def transformProgram(program: Node, compilation: core.deltas.Compilation): Unit = {
    transformClass(program)

    def transformClass(program: Node) {
      val javaClass: JavaClass[Node] = program
      JavaLang.loadIntoClassPath(compilation)
      javaClass.node.shape = ByteCodeSkeleton.Shape
      val classFile = new ClassFile(javaClass.node)
      val classCompiler: ClassCompiler = ClassCompiler(javaClass.node, compilation)
      getState(compilation).classCompiler = classCompiler
      classCompiler.bind()

      val classInfo = classCompiler.currentClassInfo
      classFile.attributes = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      program(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = javaClass.parent.get
      val parentRef = ClassInfoConstant.classRef(classCompiler.fullyQualify(parentName))
      program(ByteCodeSkeleton.ClassParentIndex) = parentRef
      program(ByteCodeSkeleton.ClassInterfaces) = Seq()

      for(member <- getRegistry(compilation).members)
        member.compile(compilation, javaClass.node)

      javaClass.node.data.remove(Members)
    }
  }

  def fullyQualify(_type: Path, classCompiler: ClassCompiler): Unit =  _type.shape match {
    case ArrayTypeDelta.ArrayTypeKey => fullyQualify(ArrayTypeDelta.getElementType(_type), classCompiler)
    case UnqualifiedObjectTypeDelta.Shape =>
        val newName = classCompiler.fullyQualify(UnqualifiedObjectTypeDelta.getName(_type))
      _type.asInstanceOf[ChildPath].replaceWith(QualifiedObjectTypeDelta.neww(newName))
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


  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: Path, defaultPackageScope: Scope): Declaration = {
    val clazz: JavaClass[Path] = path

    val packageScope = if (clazz._package.isEmpty) {
      defaultPackageScope
    } else {
      val packageParts = clazz.node._package.toList
      val fullPackage: String = packageParts.reduce[String]((a, b) => a + "." + b)
      getState(compilation).packageScopes.getOrElseUpdate(fullPackage, {
        val packageDeclaration = builder.declare(fullPackage, path, defaultPackageScope)
        builder.declareScope(packageDeclaration, Some(defaultPackageScope), fullPackage )
      })
    }

    val clazzDeclaration = new NamedDeclaration(clazz.name, path)
    val classExternalScope = builder.newScope(Some(defaultPackageScope), "externalFor" + clazz.name)
    builder.add(DeclarationInsideScope(clazzDeclaration, classExternalScope))
    builder.add(DeclarationOfScope(clazzDeclaration, classExternalScope))
    builder.importScope(packageScope, classExternalScope)

    val classInternalScope = builder.newScope(Some(classExternalScope), "internalFor" + clazz.name)

    val members = clazz.members
    members.foreach(member => hasDeclarations.get(compilation, member.shape).
      getDeclaration(compilation, builder, member, classInternalScope))

    clazzDeclaration
  }

  object ImportGrammar extends GrammarKey
  object Shape extends NodeShape

  val hasDeclarations: ShapeAspect[HasDeclaration] = new ShapeAspect[HasDeclaration]

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
    var packageScopes: mutable.Map[String, Scope] = mutable.Map.empty
  }

  object ClassGrammar

  object ClassPackage extends NodeField

  object ClassImports extends NodeField

  object ClassParent extends NodeField

  object Members extends NodeField

  object ClassName extends NodeField

  override def inject(language: Language): Unit = {
    hasDeclarations.add(language, Shape, this)

    language.collectConstraints = (compilation, builder) => {
      val defaultPackageScope = builder.newScope(None, "defaultPackageScope")
      val clazz: JavaClass[Path] = PathRoot(compilation.program)
      val clazzDeclaration = getDeclaration(compilation, builder, clazz.node, defaultPackageScope)
      val classScope  = builder.resolveScopeDeclaration(clazzDeclaration)

      val proofs = JavaLang.getProofs(compilation, defaultPackageScope)
      builder.proofs = proofs

      for(_import <- clazz.imports)
        _import.shape.asInstanceOf[ShapeWithConstraints].collectConstraints(compilation, builder, _import, classScope)

      val members = clazz.members
      members.foreach(member =>
        this.hasDeclarations.get(language, member.shape).getDeclaration(compilation, builder, member, classScope))
    }
    super.inject(language)
  }
}
