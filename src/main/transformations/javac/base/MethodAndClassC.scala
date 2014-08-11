package transformations.javac.base

import core.grammar._
import core.transformation._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.{GrammarTransformation, ProgramTransformation}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.base.model._
import transformations.javac.statements.BlockC

import scala.collection.mutable


object MethodAndClassC extends GrammarTransformation with ProgramTransformation {

  def getReferenceKindRegistry(state: TransformationState) = getState(state).referenceKindRegistry

  def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  }

  class State() {
    val referenceKindRegistry = new GetReferenceKindRegistry()
    var classCompiler: ClassCompiler = null
  }

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
      clazz(ByteCodeSkeleton.ClassConstantPool) = classCompiler.constantPool.constants
      val methods = getMethods(clazz)
      for (method <- methods)
        bindMethod(method)

      for (method <- methods)
        MethodPart.convertMethod(method, classCompiler, state)

      def bindMethod(method: MetaObject) = {
        val methodName: String = MethodPart.getMethodName(method)
        val descriptor = MethodPart.getMethodDescriptor(method)
        classInfo.content(methodName) = new MethodInfo(descriptor, MethodPart.getMethodStatic(method))
      }
    }
  }


  def getClassCompiler(state: TransformationState) = getState(state).classCompiler

  def getQualifiedClassName(clazz: MetaObject): QualifiedClassName = {
    val className = getClassName(clazz)
    new QualifiedClassName(getPackage(clazz) ++ Seq(className))
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames)

  override def transformGrammars(grammars: GrammarCatalogue) {
    MethodPart.transformGrammars(grammars)
    val classMethod = grammars.find(MethodPart.MethodGrammar)

    val classMember: Grammar = classMethod
    // val _import = "import" ~> identifier.someSeparated(".") <~ ";"
    val importsP: Grammar = produce(Seq.empty[JavaImport]) //success_import*
    val packageP = (keyword("package") ~> identifier.someSeparated(".") <~ ";") | produce(Seq.empty)
    val _classContent = "class" ~> identifier ~ ("{" ~> (classMember *) <~ "}")
    val classGrammar = grammars.create(ClassGrammar, packageP ~ importsP ~ _classContent ^^ {
      case (_package seqr _imports) seqr (name seqr members) =>
        val methods = members
        clazz(_package.asInstanceOf[Seq[String]],
          name.asInstanceOf[String],
          methods.asInstanceOf[Seq[MetaObject]],
          _imports.asInstanceOf[List[JavaImport]], None)
    })
    grammars.find(ProgramGrammar).inner = classGrammar
  }

  class GetReferenceKindRegistry extends mutable.HashMap[AnyRef, MetaObject => ReferenceKind]

  object ClassGrammar


  def getPackage(clazz: MetaObject): Seq[String] = clazz(ClassPackage).asInstanceOf[Seq[String]]

  def getImports(clazz: MetaObject) = clazz(ClassImports).asInstanceOf[List[JavaImport]]

  def clazz(_package: Seq[String], name: String, methods: Seq[MetaObject] = Seq(), imports: List[JavaImport] = List(), mbParent: Option[String] = None) = new MetaObject(ByteCodeSkeleton.ClassFileKey) {
    data.put(ByteCodeSkeleton.ClassMethodsKey, methods.toBuffer)
    data.put(ClassPackage, _package)
    data.put(ClassName, name)
    data.put(ClassImports, imports)
    mbParent match {
      case Some(parent) => data.put(ClassParent, parent)
      case _ =>
    }
  }

  def getParent(clazz: MetaObject): Option[String] = clazz.data.get(ClassParent).map(a => a.asInstanceOf[String])

  def getClassName(clazz: MetaObject) = clazz(ClassName).asInstanceOf[String]

  def getMethods(clazz: MetaObject) = clazz(ByteCodeSkeleton.ClassMethodsKey).asInstanceOf[mutable.Buffer[MetaObject]]

  object ClassPackage

  object ClassImports

  object ClassParent

  object ClassName

}
