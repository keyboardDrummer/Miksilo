package transformations.javac.base

import core.grammar._
import core.transformation.TransformationManager.ProgramGrammar
import core.transformation._
import transformations.bytecode.ByteCode._
import transformations.bytecode.{ByteCode, InferredMaxStack, InferredStackFrames}
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model.JavaTypes._
import transformations.javac.base.model._
import transformations.javac.statements.{BlockC, StatementC}
import transformations.javac.types.TypeC

import scala.collection.mutable


object JavaMethodC extends GrammarTransformation {

  def getTypeSize(_type: MetaObject): Int = _type.clazz match {
    case IntTypeKey => 1
    case BooleanTypeKey => 1
    case DoubleTypeKey => 1
    case ArrayTypeKey => 1
    case ObjectTypeKey => 1
    case ArrayTypeKey => 1
    case VoidTypeKey => 0
  }

  def getReferenceKindRegistry(state: TransformationState) = getState(state).referenceKindRegistry

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      val classCompiler = new ClassCompiler(clazz, state)
      val classInfo = classCompiler.currentClassInfo
      clazz(ByteCode.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      clazz(ByteCode.ClassNameIndexKey) = classRef
      val parentName = JavaClassModel.getParent(clazz).get
      val parentRef = classCompiler.getClassRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCode.ClassParentIndex) = parentRef
      clazz(ByteCode.ClassInterfaces) = Seq()
      clazz(ByteCode.ClassFields) = Seq()
      clazz(ByteCode.ClassConstantPool) = classCompiler.constantPool.constants
      val methods = JavaClassModel.getMethods(clazz)
      for (method <- methods)
        bindMethod(method)

      for (method <- methods)
        convertMethod(method)

      def bindMethod(method: MetaObject) = {
        val methodName: String = JavaMethodModel.getMethodName(method)
        val descriptor = getMethodDescriptor(method)
        classInfo.content(methodName) = new MethodInfo(descriptor, JavaMethodModel.getMethodStatic(method))
      }

      def addCodeAnnotation(method: MetaObject) {
        val parameters = JavaMethodModel.getMethodParameters(method)
        setMethodCompiler(method, parameters)
        val statements = JavaMethodModel.getMethodBody(method)
        val statementToInstructions = StatementC.getToInstructions(state)
        val instructions = statements.flatMap(statement => statementToInstructions(statement))
        val codeIndex = classCompiler.constantPool.store(ByteCode.CodeAttributeId)
        val exceptionTable = Seq[MetaObject]()
        val codeAttributes = Seq[MetaObject]()
        val codeAttribute = new MetaObject(ByteCode.CodeKey) {
          data.put(AttributeNameKey, codeIndex)
          data.put(CodeMaxLocalsKey, getMethodCompiler(state).variables.localCount)
          data.put(CodeInstructionsKey, instructions)
          data.put(CodeExceptionTableKey, exceptionTable)
          data.put(CodeAttributesKey, codeAttributes)
        }
        method(ByteCode.MethodAnnotations) = Seq(codeAttribute)
      }

      def setMethodCompiler(method: MetaObject, parameters: Seq[MetaObject]) {
        val methodCompiler = new MethodCompiler(classCompiler)
        if (!JavaMethodModel.getMethodStatic(method))
          methodCompiler.variables.add("this", JavaTypes.objectType(classCompiler.currentClassInfo.name))
        for (parameter <- parameters)
          methodCompiler.variables.add(JavaMethodModel.getParameterName(parameter), JavaMethodModel.getParameterType(parameter))
        getState(state).methodCompiler = methodCompiler
      }

      def convertMethod(method: MetaObject) {
        addMethodFlags(method)
        val methodNameIndex: Int = classCompiler.getMethodNameIndex(JavaMethodModel.getMethodName(method))
        method(ByteCode.MethodNameIndex) = methodNameIndex
        method.data.remove(JavaMethodModel.MethodNameKey)
        val methodDescriptorIndex = getMethodDescriptorIndex(method)
        method(ByteCode.MethodDescriptorIndex) = methodDescriptorIndex
        addCodeAnnotation(method)
        method.data.remove(JavaMethodModel.ReturnTypeKey)
        method.data.remove(JavaMethodModel.MethodParametersKey)
      }

      def getMethodDescriptorIndex(method: MetaObject): Int = classCompiler.constantPool.store(getMethodDescriptor(method))
      def getMethodDescriptor(method: MetaObject): MetaObject = {
        val returnType = JavaMethodModel.getMethodReturnType(method)
        val parameters = JavaMethodModel.getMethodParameters(method)
        ByteCode.methodDescriptor(returnType, parameters.map(p => JavaMethodModel.getParameterType(p)))
      }
    }
  }

  def addMethodFlags(method: MetaObject) = {
    var flags = Set[ByteCode.MethodAccessFlag]()
    if (JavaMethodModel.getMethodStatic(method))
      flags += ByteCode.StaticAccess

    JavaMethodModel.getMethodVisibility(method) match {
      case JavaMethodModel.PublicVisibility => flags += ByteCode.PublicAccess
      case JavaMethodModel.PrivateVisibility => flags += ByteCode.PrivateAccess
    }

    method(ByteCode.MethodAccessFlags) = flags
  }

  def getMethodCompiler(state: TransformationState) = getState(state).methodCompiler

  def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  }

  def getQualifiedClassName(clazz: MetaObject): QualifiedClassName = {
    val className = JavaClassModel.getClassName(clazz)
    new QualifiedClassName(JavaClassModel.getPackage(clazz) ++ Seq(className))
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames)

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq("(", ")", "{", "}", "[", "]", "[]")

  override def transformReserved(reserved: mutable.HashSet[String]): Unit =
    reserved ++= Seq("void", "class", "package", "public", "static", "int")

  override def transformGrammars(grammars: GrammarCatalogue) {
    val block = grammars.find(BlockC.BlockGrammar)

    val parseType = grammars.find(TypeC.TypeGrammar)
    val parseReturnType = "void" ^^ (_ => JavaTypes.voidType) | parseType

    val parseParameter = parseType ~ identifier ^^ {
      case _type seqr _name => JavaMethodModel.parameter(_name.asInstanceOf[String], _type)
    }
    val parseParameters = "(" ~> parseParameter.someSeparated(",") <~ ")"
    val parseStatic = "static" ^^ (_ => true) | produce(false)
    val visibilityModifier =
      "public" ^^ (_ => PublicVisibility) |
        "protected" ^^ (_ => ProtectedVisibility) |
        "private" ^^ (_ => PrivateVisibility) |
        produce(DefaultVisibility)
    val classMethod = grammars.create(MethodGrammar, visibilityModifier ~ parseStatic ~ parseReturnType ~ identifier ~
      parseParameters ~ block ^^ {
      case visibility seqr static seqr returnType seqr name seqr parameters seqr body =>
        JavaMethodModel.method(name.asInstanceOf[String], returnType, parameters.asInstanceOf[Seq[MetaObject]], body.asInstanceOf[Seq[MetaObject]],
          static.asInstanceOf[Boolean], visibility.asInstanceOf[Visibility])
    })

    val classMember: Grammar = classMethod
    // val _import = "import" ~> identifier.someSeparated(".") <~ ";"
    val importsP: Grammar = produce(Seq.empty[JavaImport]) //success_import*
    val packageP = keyword("package") ~> identifier.someSeparated(".") <~ ";"
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

  class State() {
    val referenceKindRegistry = new GetReferenceKindRegistry()
    var methodCompiler: MethodCompiler = null
  }

  object TypeGrammar

  object ClassGrammar

  object MethodGrammar

}
