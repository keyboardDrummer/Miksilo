package transformations.javac.classes

import core.bigrammar.{BiGrammar, MapGrammar}
import core.document.BlankLine
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ClassFileKey
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.statements.BlockC
import transformations.types.{ArrayTypeC, ObjectTypeC}


object JavaClassSkeleton extends ParticleWithGrammar with ParticleWithPhase with WithState {


  override def transform(program: Node, state: CompilationState): Unit = {
    transformClass(program)

    def transformClass(clazz: Node) {
      val classCompiler: ClassCompiler = initializeClassCompiler(state, clazz)
      
      val classInfo = classCompiler.currentClassInfo
      clazz(ByteCodeSkeleton.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      clazz(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = getParent(clazz).get
      val parentRef = classCompiler.constantPool.getClassRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCodeSkeleton.ClassParentIndex) = parentRef
      clazz(ByteCodeSkeleton.ClassInterfaces) = Seq()
      clazz(ByteCodeSkeleton.ClassConstantPool) = classCompiler.constantPool

      for(secondMemberPass <- getState(state).secondMemberPasses)
        secondMemberPass(clazz)

      clazz.data.remove(Members)
    }
  }

  def initializeClassCompiler(state: CompilationState, clazz: Node): ClassCompiler = {
    val classCompiler = new ClassCompiler(clazz, state)
    getState(state).classCompiler = classCompiler

    for (firstMemberPass <- getState(state).firstMemberPasses)
      firstMemberPass(clazz)
    classCompiler
  }

  def fullyQualify(_type: Node, classCompiler: ClassCompiler): Unit =  _type.clazz match {
    case ArrayTypeC.ArrayTypeKey => fullyQualify(ArrayTypeC.getArrayElementType(_type), classCompiler)
    case ObjectTypeC.ObjectTypeKey =>
      val newName = ObjectTypeC.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeC.ObjectTypeName) = newName
    case _ =>
  }

  def getParent(clazz: Node): Option[String] = clazz.data(ClassParent).asInstanceOf[Option[String]]

  def getClassCompiler(state: CompilationState) = getState(state).classCompiler

  def getQualifiedClassName(clazz: Node): QualifiedClassName = {
    val className = getClassName(clazz)
    new QualifiedClassName(getPackage(clazz) ++ Seq(className))
  }

  def getPackage(clazz: Node): Seq[String] = clazz(ClassPackage).asInstanceOf[Seq[String]]

  def getClassName(clazz: Node) = clazz(ClassName).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames)

  object ClassMemberGrammar
  override def transformGrammars(grammars: GrammarCatalogue) {

    val classMember: BiGrammar = grammars.create(ClassMemberGrammar)
    val importGrammar = grammars.create(ImportGrammar)
    val importsGrammar: BiGrammar = importGrammar.manyVertical
    val packageGrammar = (keyword("package") ~> identifier.someSeparated(".") <~ ";") | produce(Seq.empty)
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

  def getImports(clazz: Node) = clazz(ClassImports).asInstanceOf[Seq[Node]]

  def createState = new State()
  class State() {
    var classCompiler: ClassCompiler = null
    val importToClassMap = new ClassRegistry[Node => Map[String, QualifiedClassName]]()
    var firstMemberPasses = List.empty[Node => Unit]
    var secondMemberPasses = List.empty[Node => Unit]
  }

  def getMembers(clazz: Node) = clazz(Members).asInstanceOf[Seq[Node]]

  object ClassGrammar

  object ClassPackage

  object ClassImports

  object ClassParent

  object Members

  object ClassName

  override def description: String = "Defines a skeleton for the Java class."
}
