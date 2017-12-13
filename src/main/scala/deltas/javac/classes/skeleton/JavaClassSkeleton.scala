package deltas.javac.classes.skeleton

import core.bigrammar.BiGrammar
import core.document.BlankLine
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node.{GrammarKey, Node, NodeField, NodeLike}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.Shape
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.bytecode.types.{ArrayTypeC, ObjectTypeDelta}
import deltas.javac.JavaLang
import deltas.javac.classes.ClassCompiler
import deltas.javac.statements.BlockDelta

object JavaClassSkeleton extends DeltaWithGrammar with DeltaWithPhase
  with WithLanguageRegistry with WithCompilationState {

  implicit class JavaClass[T <: NodeLike](val node: T) extends AnyVal {
    def _package = node(ClassPackage).asInstanceOf[Seq[String]]
    def _package_=(value: Seq[String]) = node(ClassPackage) = value

    def imports = node(ClassImports).asInstanceOf[Seq[T]]
    def imports_=(value: Seq[T]) = node(ClassImports) = value

    def name = node(ClassName).asInstanceOf[String]
    def name_=(value: String) = node(ClassName) = value

    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]) = node(Members) = value

    def parent = node(ClassParent).asInstanceOf[Option[String]]
    def parent_=(value: Option[String]) = node(ClassParent) = value
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    transformClass(program)

    def transformClass(javaClass: Node) {
      JavaLang.loadIntoClassPath(compilation)
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
    case ArrayTypeC.ArrayTypeKey => fullyQualify(ArrayTypeC.getArrayElementType(_type), classCompiler)
    case ObjectTypeDelta.ObjectTypeKey =>
        val newName = ObjectTypeDelta.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeDelta.Name) = newName
    case _ =>
  }

  def getClassCompiler(compilation: Compilation) = getState(compilation).classCompiler

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
    val membersGrammar = "{" % (classMember.manySeparatedVertical(BlankLine) as Members).indent(BlockDelta.indentAmount) % "}"
    val nameAndParent: BiGrammar = nameGrammar.as(ClassName) ~~ classParentGrammar.as(ClassParent)
    val classGrammar = packageGrammar % importsGrammar % nameAndParent % membersGrammar asLabelledNode Shape
    find(BodyGrammar).inner = classGrammar
  }

  object ImportGrammar extends GrammarKey

  def neww(_package: Seq[String], name: String, members: Seq[Node] = Seq(), imports: List[Node] = List(), mbParent: Option[String] = None) =
    new Node(ByteCodeSkeleton.Shape,
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

  override def description: String = "Defines a skeleton for the Java class."
}
