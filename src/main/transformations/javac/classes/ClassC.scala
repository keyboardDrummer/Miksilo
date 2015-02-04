package transformations.javac.classes

import core.document.BlankLine
import core.grammar.~
import core.grammarDocument.BiGrammar
import core.transformation._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.{GrammarTransformation, ProgramTransformation}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.ClassFileKey
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.methods.MethodC
import transformations.javac.statements.BlockC
import transformations.types.{ArrayTypeC, ObjectTypeC}

import scala.collection.mutable


object ClassC extends GrammarTransformation with ProgramTransformation {

  def getReferenceKindRegistry(state: TransformationState) = getState(state).referenceKindRegistry

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
      clazz(ByteCodeSkeleton.ClassFields) = Seq()
      clazz(ByteCodeSkeleton.ClassConstantPool) = classCompiler.constantPool

      val methods = getMethods(clazz)
      for (method <- methods)
        bindMethod(method)

      for (method <- methods)
        MethodC.convertMethod(method, classCompiler, state)

      def bindMethod(method: MetaObject) = {
        val methodName: String = MethodC.getMethodName(method)
        val descriptor = MethodC.getMethodDescriptor(method, classCompiler)
        classInfo.content(methodName) = new MethodInfo(descriptor, MethodC.getMethodStatic(method))
      }
    }
  }

  def fullyQualify(_type: MetaObject, classCompiler: ClassCompiler): Unit =  _type.clazz match {
    case ArrayTypeC.ArrayTypeKey => fullyQualify(ArrayTypeC.getArrayElementType(_type), classCompiler)
    case ObjectTypeC.ObjectTypeKey =>
      val newName = ObjectTypeC.getObjectTypeName(_type).left.flatMap(inner => Right(classCompiler.fullyQualify(inner)))
      _type(ObjectTypeC.ObjectTypeName) = newName
    case _ =>
  }

  def getParent(clazz: MetaObject): Option[String] = clazz.data.get(ClassParent).map(a => a.asInstanceOf[String])

  def getMethods(clazz: MetaObject) = clazz(ByteCodeSkeleton.ClassMethodsKey).asInstanceOf[Seq[MetaObject]]

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

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames, MethodC)

  object ClassMemberGrammar
  override def transformGrammars(grammars: GrammarCatalogue) {
    val classMethod = grammars.find(MethodC.MethodGrammar)

    val classMember: BiGrammar = grammars.create(ClassMemberGrammar, classMethod)
    val importGrammar = grammars.create(ImportGrammar)
    val importsGrammar: BiGrammar = importGrammar.manyVertical
    val packageGrammar = (keyword("package") ~> identifier.someSeparated(".") <~ ";") | produce(Seq.empty)
    val _classContent = "class" ~~> identifier % ("{" %> classMember.manySeparatedVertical(BlankLine).indent(BlockC.indentAmount) %< "}")
    val classGrammar = grammars.create(ClassGrammar, packageGrammar % importsGrammar % _classContent ^^
      (
        { case (_package ~ imports) ~ (name ~ methods) => core.grammar.~(core.grammar.~(core.grammar.~(_package, imports), name), methods) },
        { case _package ~ imports ~ name ~ methods => Some(core.grammar.~(core.grammar.~(_package, imports), core.grammar.~(name, methods))) }) ^^
      parseMap(ClassFileKey, ClassPackage, ClassImports, ClassName, ByteCodeSkeleton.ClassMethodsKey))
    grammars.find(ProgramGrammar).inner = classGrammar
  }

  object ImportGrammar

  def clazz(_package: Seq[String], name: String, methods: Seq[MetaObject] = Seq(), imports: List[MetaObject] = List(), mbParent: Option[String] = None) =
    new MetaObject(ByteCodeSkeleton.ClassFileKey) {
    data.put(ByteCodeSkeleton.ClassMethodsKey, methods)
    data.put(ClassPackage, _package)
    data.put(ClassName, name)
    data.put(ClassImports, imports)
    mbParent match {
      case Some(parent) => data.put(ClassParent, parent)
      case _ =>
    }
  }

  def getImports(clazz: MetaObject) = clazz(ClassImports).asInstanceOf[Seq[MetaObject]]

  class State() {
    val referenceKindRegistry = new GetReferenceKindRegistry()
    var classCompiler: ClassCompiler = null
    val importToClassMap = new mutable.HashMap[AnyRef, MetaObject => Map[String, QualifiedClassName]]()
  }

  class GetReferenceKindRegistry extends mutable.HashMap[AnyRef, MetaObject => ReferenceKind]

  object ClassGrammar

  object ClassPackage

  object ClassImports

  object ClassParent

  object ClassName

}
