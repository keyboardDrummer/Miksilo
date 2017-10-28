package transformations.javac.classes.skeleton

import core.bigrammar.BiGrammar
import core.document.BlankLine
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{GrammarKey, Node, NodeField, NodeLike}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.Clazz
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.bytecode.types.{ArrayTypeC, ObjectTypeDelta}
import transformations.javac.JavaLang
import transformations.javac.classes.ClassCompiler
import transformations.javac.statements.BlockC

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

  override def transform(program: Node, compilation: Compilation): Unit = {
    transformClass(program)

    def transformClass(clazz: Node) {
      JavaLang.loadIntoClassPath(compilation)
      val classCompiler: ClassCompiler = ClassCompiler(clazz, compilation)
      getState(compilation).classCompiler = classCompiler
      classCompiler.bind()

      val classInfo = classCompiler.currentClassInfo
      clazz(ByteCodeSkeleton.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      clazz(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = clazz.parent.get
      val parentRef = ClassInfoConstant.classRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCodeSkeleton.ClassParentIndex) = parentRef
      clazz(ByteCodeSkeleton.ClassInterfaces) = Seq()

      for(member <- getRegistry(compilation).members)
        member.compile(compilation, clazz)

      clazz.data.remove(Members)
    }
  }

  def fullyQualify(_type: Node, classCompiler: ClassCompiler): Unit =  _type.clazz match {
    case ArrayTypeC.ArrayTypeKey => fullyQualify(ArrayTypeC.getArrayElementType(_type), classCompiler)
    case ObjectTypeDelta.ObjectTypeKey =>
        val newName = ObjectTypeDelta.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeDelta.Name) = newName
    case _ =>
  }

  def getClassCompiler(compilation: Compilation) = getState(compilation).classCompiler

  def getQualifiedClassName(clazz: Node): QualifiedClassName = {
    QualifiedClassName(clazz._package ++ Seq(clazz.name))
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames)

  object ClassMemberGrammar extends GrammarKey
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {

    val classMember: BiGrammar = grammars.create(ClassMemberGrammar)
    val importGrammar = grammars.create(ImportGrammar)
    val importsGrammar: BiGrammar = importGrammar.manyVertical as ClassImports
    val packageGrammar = (keyword("package") ~~> identifier.someSeparated(".") ~< ";") | value(Seq.empty) as ClassPackage
    val classParentGrammar = ("extends" ~~> identifier).option
    val nameGrammar: BiGrammar = "class" ~~> identifier
    val membersGrammar = "{" %> classMember.manySeparatedVertical(BlankLine).indent(BlockC.indentAmount) %< "}" as Members
    val nameAndParent: BiGrammar = nameGrammar.as(ClassName) ~~ classParentGrammar.as(ClassParent)
    val classGrammar = grammars.create(Clazz, packageGrammar % importsGrammar % nameAndParent % membersGrammar asNode Clazz)
    grammars.find(ProgramGrammar).inner = classGrammar
  }

  object ImportGrammar extends GrammarKey

  def clazz(_package: Seq[String], name: String, members: Seq[Node] = Seq(), imports: List[Node] = List(), mbParent: Option[String] = None) =
    new Node(ByteCodeSkeleton.Clazz,
    Members -> members,
    ClassPackage -> _package,
    ClassName -> name,
    ClassImports -> imports,
    ClassParent -> mbParent)

  def createRegistry = new Registry()
  class Registry {
    var members = List.empty[ClassMemberDelta]
    val importToClassMap = new ClassRegistry[(Compilation, Node) => Map[String, QualifiedClassName]]()
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
