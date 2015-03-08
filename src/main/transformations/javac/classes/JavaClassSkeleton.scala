package transformations.javac.classes

import core.document.BlankLine
import core.grammarDocument.{BiGrammar, MapGrammar}
import core.transformation._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.{GrammarTransformation, ParticleWithPhase}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ClassFileKey
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.statements.BlockC
import transformations.types.{ArrayTypeC, ObjectTypeC}

import scala.collection.mutable


object JavaClassSkeleton extends GrammarTransformation with ParticleWithPhase {

  def getReferenceKindRegistry(state: TransformationState) = getState(state).referenceKindRegistry //TODO move this registry to SelectorC.

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      val classCompiler = new ClassCompiler(clazz, state)
      getState(state).classCompiler = classCompiler

      val classInfo = classCompiler.currentClassInfo
      clazz(ByteCodeSkeleton.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      clazz(ByteCodeSkeleton.ClassNameIndexKey) = classRef
      val parentName = getParent(clazz).get
      val parentRef = classCompiler.constantPool.getClassRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCodeSkeleton.ClassParentIndex) = parentRef
      clazz(ByteCodeSkeleton.ClassInterfaces) = Seq()
      clazz(ByteCodeSkeleton.ClassConstantPool) = classCompiler.constantPool

      for(firstMemberPass <- getState(state).firstMemberPasses)
        firstMemberPass(clazz)

      for(secondMemberPass <- getState(state).secondMemberPasses)
        secondMemberPass(clazz)

      clazz.data.remove(Members)
    }
  }

  def fullyQualify(_type: MetaObject, classCompiler: ClassCompiler): Unit =  _type.clazz match {
    case ArrayTypeC.ArrayTypeKey => fullyQualify(ArrayTypeC.getArrayElementType(_type), classCompiler)
    case ObjectTypeC.ObjectTypeKey =>
      val newName = ObjectTypeC.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeC.ObjectTypeName) = newName
    case _ =>
  }

  def getParent(clazz: MetaObject): Option[String] = clazz.data(ClassParent).asInstanceOf[Option[String]]

  def getClassCompiler(state: TransformationState) = getState(state).classCompiler

  def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  }

  def getQualifiedClassName(clazz: MetaObject): QualifiedClassName = {
    val className = getClassName(clazz)
    new QualifiedClassName(getPackage(clazz) ++ Seq(className))
  }

  def getPackage(clazz: MetaObject): Seq[String] = clazz(ClassPackage).asInstanceOf[Seq[String]]

  def getClassName(clazz: MetaObject) = clazz(ClassName).asInstanceOf[String]

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

  def clazz(_package: Seq[String], name: String, members: Seq[MetaObject] = Seq(), imports: List[MetaObject] = List(), mbParent: Option[String] = None) =
    new MetaObject(ByteCodeSkeleton.ClassFileKey) {
    data.put(Members, members)
    data.put(ClassPackage, _package)
    data.put(ClassName, name)
    data.put(ClassImports, imports)
    data.put(ClassParent, mbParent)
  }

  def getImports(clazz: MetaObject) = clazz(ClassImports).asInstanceOf[Seq[MetaObject]]

  class State() {
    val referenceKindRegistry = new GetReferenceKindRegistry()
    var classCompiler: ClassCompiler = null
    val importToClassMap = new mutable.HashMap[AnyRef, MetaObject => Map[String, QualifiedClassName]]()
    var firstMemberPasses = List.empty[MetaObject => Unit]
    var secondMemberPasses = List.empty[MetaObject => Unit]
  }

  def getMembers(clazz: MetaObject) = clazz(Members).asInstanceOf[Seq[MetaObject]]

  class GetReferenceKindRegistry extends mutable.HashMap[AnyRef, MetaObject => ReferenceKind]

  object ClassGrammar

  object ClassPackage

  object ClassImports

  object ClassParent

  object Members

  object ClassName

  override def description: String = "Defines a skeleton for the Java class."
}
