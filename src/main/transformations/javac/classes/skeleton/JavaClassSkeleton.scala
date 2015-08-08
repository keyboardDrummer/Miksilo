package transformations.javac.classes.skeleton

import core.bigrammar.{BiGrammar, MapGrammar}
import core.document.BlankLine
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ClassFileKey
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.bytecode.types.{ArrayTypeC, ObjectTypeC}
import transformations.javac.JavaLang
import transformations.javac.classes.ClassCompiler
import transformations.javac.statements.BlockC



object JavaClassSkeleton extends ParticleWithGrammar with ParticleWithPhase with WithState {

  implicit class JavaClass(val node: Node) extends AnyVal {
    def _package = node(ClassPackage).asInstanceOf[Seq[String]]
    def _package_=(value: Seq[String]) = node(ClassPackage) = value

    def imports = node(ClassImports).asInstanceOf[Seq[Node]]
    def imports_=(value: Seq[Node]) = node(ClassImports) = value

    def name = node(ClassName).asInstanceOf[String]
    def name_=(value: String) = node(ClassName) = value

    def members = node(Members).asInstanceOf[Seq[Node]]
    def members_=(value: Seq[Node]) = node(Members) = value

    def parent = node(ClassParent).asInstanceOf[Option[String]]
    def parent_=(value: Option[String]) = node(ClassParent) = value
  }

  override def transform(program: Node, state: CompilationState): Unit = {
    transformClass(program)

    def transformClass(clazz: Node) {
      val compiler = new MyCompiler(state)
      JavaLang.initialise(compiler)
      val classCompiler: ClassCompiler = new ClassCompiler(clazz, compiler)
      
      val classInfo = classCompiler.currentClassInfo
      clazz(ByteCodeSkeleton.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      clazz(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = clazz.parent.get
      val parentRef = classCompiler.constantPool.getClassRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCodeSkeleton.ClassParentIndex) = parentRef
      clazz(ByteCodeSkeleton.ClassInterfaces) = Seq()
      clazz(ByteCodeSkeleton.ClassConstantPool) = classCompiler.constantPool

      for(member <- getState(state).members)
        member.compile(state, clazz)

      clazz.data.remove(Members)
    }
  }

  def fullyQualify(_type: Node, classCompiler: ClassCompiler): Unit =  _type.clazz match {
    case ArrayTypeC.ArrayTypeKey => fullyQualify(ArrayTypeC.getArrayElementType(_type), classCompiler)
    case ObjectTypeC.ObjectTypeKey =>
      val newName = ObjectTypeC.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeC.ObjectTypeName) = newName
    case _ =>
  }

  def getClassCompiler(state: CompilationState) = getState(state).classCompiler

  def getQualifiedClassName(clazz: Node): QualifiedClassName = {
    new QualifiedClassName(clazz._package ++ Seq(clazz.name))
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames)

  object ClassMemberGrammar
  override def transformGrammars(grammars: GrammarCatalogue) {

    val classMember: BiGrammar = grammars.create(ClassMemberGrammar)
    val importGrammar = grammars.create(ImportGrammar)
    val importsGrammar: BiGrammar = importGrammar.manyVertical
    val packageGrammar = (keyword("package") ~~> identifier.someSeparated(".") <~ ";") | produce(Seq.empty)
    val classParentGrammar = ("extends" ~~> identifier ^^ (x => Some(x), x => x.asInstanceOf[Option[Any]])) | produce(None)
    val nameGrammar: BiGrammar = "class" ~~> identifier
    val membersGrammar: MapGrammar = "{" %> classMember.manySeparatedVertical(BlankLine).indent(BlockC.indentAmount) %< "}"
    val nameAndParent: BiGrammar = nameGrammar ~~ classParentGrammar ^^ parseMap(ClassFileKey, ClassName, ClassParent)
    val classGrammar = grammars.create(ClassGrammar, packageGrammar % importsGrammar % nameAndParent % membersGrammar ^^
      parseMap(ClassFileKey, ClassPackage, ClassImports, PartialSelf, Members))
    grammars.find(ProgramGrammar).inner = classGrammar
  }

  object ImportGrammar

  def clazz(_package: Seq[String], name: String, members: Seq[Node] = Seq(), imports: List[Node] = List(), mbParent: Option[String] = None) =
    new Node(ByteCodeSkeleton.ClassFileKey,
    Members -> members,
    ClassPackage -> _package,
    ClassName -> name,
    ClassImports -> imports,
    ClassParent -> mbParent)

  def createState = new State()
  class State() {
    var classCompiler: ClassCompiler = null
    val importToClassMap = new ClassRegistry[Node => Map[String, QualifiedClassName]]()
    var members = List.empty[ClassMemberC]
  }

  object ClassGrammar

  object ClassPackage

  object ClassImports

  object ClassParent

  object Members

  object ClassName

  override def description: String = "Defines a skeleton for the Java class."
}
