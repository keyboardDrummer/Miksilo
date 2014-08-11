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


object MethodAndClassC extends MethodPart with GrammarTransformation with ProgramTransformation {

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
      val parentName = JavaClassModel.getParent(clazz).get
      val parentRef = classCompiler.constantPool.getClassRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCodeSkeleton.ClassParentIndex) = parentRef
      clazz(ByteCodeSkeleton.ClassInterfaces) = Seq()
      clazz(ByteCodeSkeleton.ClassFields) = Seq()
      clazz(ByteCodeSkeleton.ClassConstantPool) = classCompiler.constantPool.constants
      val methods = JavaClassModel.getMethods(clazz)
      for (method <- methods)
        bindMethod(method)

      for (method <- methods)
        convertMethod(method, classCompiler, state)

      def bindMethod(method: MetaObject) = {
        val methodName: String = JavaMethodModel.getMethodName(method)
        val descriptor = getMethodDescriptor(method)
        classInfo.content(methodName) = new MethodInfo(descriptor, JavaMethodModel.getMethodStatic(method))
      }
    }
  }


  def getClassCompiler(state: TransformationState) = getState(state).classCompiler

  def getQualifiedClassName(clazz: MetaObject): QualifiedClassName = {
    val className = JavaClassModel.getClassName(clazz)
    new QualifiedClassName(JavaClassModel.getPackage(clazz) ++ Seq(className))
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames)

  override def transformGrammars(grammars: GrammarCatalogue) {
    super.transformGrammars(grammars)
    val classMethod = grammars.find(this.MethodGrammar)

    val classMember: Grammar = classMethod
    // val _import = "import" ~> identifier.someSeparated(".") <~ ";"
    val importsP: Grammar = produce(Seq.empty[JavaImport]) //success_import*
    val packageP = (keyword("package") ~> identifier.someSeparated(".") <~ ";") | produce(Seq.empty)
    val _classContent = "class" ~> identifier ~ ("{" ~> (classMember *) <~ "}")
    val classGrammar = grammars.create(ClassGrammar, packageP ~ importsP ~ _classContent ^^ {
      case (_package seqr _imports) seqr (name seqr members) =>
        val methods = members
        JavaClassModel.clazz(_package.asInstanceOf[Seq[String]],
          name.asInstanceOf[String],
          methods.asInstanceOf[Seq[MetaObject]],
          _imports.asInstanceOf[List[JavaImport]], None)
    })
    grammars.find(ProgramGrammar).inner = classGrammar
  }

  class GetReferenceKindRegistry extends mutable.HashMap[AnyRef, MetaObject => ReferenceKind]

  object ClassGrammar

}
