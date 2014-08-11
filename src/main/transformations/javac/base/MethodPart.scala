package transformations.javac.base

import core.grammar.seqr
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.javac.base.model.JavaMethodModel
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.statements.{BlockC, StatementC}
import transformations.javac.types.{ObjectTypeC, TypeC, VoidTypeC}

class MethodPart extends GrammarTransformation {

  class State() {
    var methodCompiler: MethodCompiler = null
  }

  private def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(classOf[MethodPart], new State()).asInstanceOf[State]
  }

  def getMethodDescriptor(method: MetaObject): MetaObject = {
    val returnType = JavaMethodModel.getMethodReturnType(method)
    val parameters = JavaMethodModel.getMethodParameters(method)
    ByteCodeSkeleton.methodDescriptor(returnType, parameters.map(p => JavaMethodModel.getParameterType(p)))
  }

  def convertMethod(method: MetaObject, classCompiler: ClassCompiler, state: TransformationState) {

    def getMethodDescriptorIndex(method: MetaObject): Int = classCompiler.constantPool.store(getMethodDescriptor(method))

    addMethodFlags(method)
    val methodNameIndex: Int = classCompiler.getMethodNameIndex(JavaMethodModel.getMethodName(method))
    method(ByteCodeSkeleton.MethodNameIndex) = methodNameIndex
    method.data.remove(JavaMethodModel.MethodNameKey)
    val methodDescriptorIndex = getMethodDescriptorIndex(method)
    method(ByteCodeSkeleton.MethodDescriptorIndex) = methodDescriptorIndex
    addCodeAnnotation(method)
    method.data.remove(JavaMethodModel.ReturnTypeKey)
    method.data.remove(JavaMethodModel.MethodParametersKey)

    def addCodeAnnotation(method: MetaObject) {
      val parameters = JavaMethodModel.getMethodParameters(method)
      setMethodCompiler(method, parameters)
      val statements = JavaMethodModel.getMethodBody(method)
      val statementToInstructions = StatementC.getToInstructions(state)
      val instructions = statements.flatMap(statement => statementToInstructions(statement))
      val codeIndex = classCompiler.constantPool.store(ByteCodeSkeleton.CodeAttributeId)
      val exceptionTable = Seq[MetaObject]()
      val codeAttributes = Seq[MetaObject]()
      val codeAttribute = new MetaObject(ByteCodeSkeleton.CodeKey) {
        data.put(AttributeNameKey, codeIndex)
        data.put(CodeMaxLocalsKey, getMethodCompiler(state).variables.localCount)
        data.put(CodeInstructionsKey, instructions)
        data.put(CodeExceptionTableKey, exceptionTable)
        data.put(CodeAttributesKey, codeAttributes)
      }
      method(ByteCodeSkeleton.MethodAnnotations) = Seq(codeAttribute)
    }

    def setMethodCompiler(method: MetaObject, parameters: Seq[MetaObject]) {
      val methodCompiler = new MethodCompiler(state)
      if (!JavaMethodModel.getMethodStatic(method))
        methodCompiler.variables.add("this", ObjectTypeC.objectType(classCompiler.currentClassInfo.name))
      for (parameter <- parameters)
        methodCompiler.variables.add(JavaMethodModel.getParameterName(parameter), JavaMethodModel.getParameterType(parameter))
      getState(state).methodCompiler = methodCompiler
    }
  }

  def addMethodFlags(method: MetaObject) = {
    var flags = Set[ByteCodeSkeleton.MethodAccessFlag]()
    if (JavaMethodModel.getMethodStatic(method))
      flags += ByteCodeSkeleton.StaticAccess

    JavaMethodModel.getMethodVisibility(method) match {
      case JavaMethodModel.PublicVisibility => flags += ByteCodeSkeleton.PublicAccess
      case JavaMethodModel.PrivateVisibility => flags += ByteCodeSkeleton.PrivateAccess
    }

    method(ByteCodeSkeleton.MethodAccessFlags) = flags
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    val block = grammars.find(BlockC.BlockGrammar)

    val parseType = grammars.find(TypeC.TypeGrammar)
    val parseReturnType = "void" ^^ (_ => VoidTypeC.voidType) | parseType

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

    grammars.create(MethodGrammar, visibilityModifier ~ parseStatic ~ parseReturnType ~ identifier ~
      parseParameters ~ block ^^ {
      case visibility seqr static seqr returnType seqr name seqr parameters seqr body =>
        JavaMethodModel.method(name.asInstanceOf[String], returnType, parameters.asInstanceOf[Seq[MetaObject]], body.asInstanceOf[Seq[MetaObject]],
          static.asInstanceOf[Boolean], visibility.asInstanceOf[Visibility])
    })
  }

  def getMethodCompiler(state: TransformationState) = getState(state).methodCompiler

  object MethodGrammar

}
