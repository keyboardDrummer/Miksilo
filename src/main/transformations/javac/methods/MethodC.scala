package transformations.javac.methods

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.attributes.{CodeConstantEntry, CodeAttribute}
import CodeAttribute.{CodeMaxLocalsKey, CodeExceptionTableKey, CodeAttributesKey, CodeInstructionsKey}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.constants.MethodDescriptorConstant
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.classes.{MethodInfo, ClassC, ClassCompiler}
import transformations.javac.statements.{BlockC, StatementC}
import transformations.types.{ObjectTypeC, TypeC, VoidTypeC}

object MethodC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    super.inject(state)

    ClassC.getState(state).firstMemberPasses ::= (clazz => bindMethods(state, clazz))
    ClassC.getState(state).secondMemberPasses ::= (clazz => convertMethods(state, clazz))
  }

  def convertMethods(state: TransformationState, clazz: MetaObject) = {
    val classCompiler = ClassC.getClassCompiler(state)

    val methods = getMethods(clazz)
    clazz(ByteCodeSkeleton.ClassMethodsKey) = methods.map(method => {
      convertMethod(method, classCompiler, state)
      method
    })
  }

  def bindMethods(state: TransformationState, clazz: MetaObject): Unit = {
    val classCompiler = ClassC.getClassCompiler(state)
    val classInfo = classCompiler.currentClassInfo

    val methods = getMethods(clazz)
    for (method <- methods)
      bindMethod(method)

    def bindMethod(method: MetaObject) = {
      val methodName: String = MethodC.getMethodName(method)
      val descriptor = MethodC.getMethodDescriptor(method, classCompiler)
      classInfo.content(methodName) = new MethodInfo(descriptor, MethodC.getMethodStatic(method))
    }
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames, ClassC)

  def getParameterType(metaObject: MetaObject, classCompiler: ClassCompiler) = {
    val result = metaObject(ParameterTypeKey).asInstanceOf[MetaObject]
    ClassC.fullyQualify(result, classCompiler)
    result
  }

  def getMethodDescriptor(method: MetaObject, classCompiler: ClassCompiler): MetaObject = {
    val returnType = getMethodReturnType(method)
    val parameters = getMethodParameters(method)
    MethodDescriptorConstant.methodDescriptor(returnType, parameters.map(p => getParameterType(p, classCompiler)))
  }

  def convertMethod(method: MetaObject, classCompiler: ClassCompiler, state: TransformationState): Unit = {
    val constantPool = ByteCodeSkeleton.getState(state).constantPool
    def getMethodDescriptorIndex(method: MetaObject): Int = constantPool.store(getMethodDescriptor(method, classCompiler))

    method.clazz = ByteCodeMethodInfo.MethodInfoKey
    addMethodFlags(method)
    val methodNameIndex: Int = classCompiler.getNameIndex(getMethodName(method))
    method(ByteCodeMethodInfo.MethodNameIndex) = methodNameIndex
    method.data.remove(MethodNameKey)
    val methodDescriptorIndex = getMethodDescriptorIndex(method)
    method(ByteCodeMethodInfo.MethodDescriptorIndex) = methodDescriptorIndex
    addCodeAnnotation(method)
    method.data.remove(ReturnTypeKey)
    method.data.remove(MethodParametersKey)

    def addCodeAnnotation(method: MetaObject) {
      val parameters = getMethodParameters(method)
      setMethodCompiler(method, parameters)
      val statements = getMethodBody(method)
      method.data.remove(MethodBodyKey)
      val statementToInstructions = StatementC.getToInstructions(state)
      val instructions = statements.flatMap(statement => statementToInstructions(statement))
      val codeIndex = constantPool.store(CodeConstantEntry.entry)
      val exceptionTable = Seq[MetaObject]()
      val codeAttributes = Seq[MetaObject]()
      val codeAttribute = new MetaObject(CodeAttribute.CodeKey,
        AttributeNameKey -> codeIndex,
        CodeMaxLocalsKey -> getMethodCompiler(state).variables.localCount,
        CodeInstructionsKey -> instructions,
        CodeExceptionTableKey -> exceptionTable,
        CodeAttributesKey -> codeAttributes)
      method(ByteCodeMethodInfo.MethodAttributes) = Seq(codeAttribute)
    }

    //TODO don't depend on classCompiler and don't get called directly from ClassC.
    def setMethodCompiler(method: MetaObject, parameters: Seq[MetaObject]) {
      val methodCompiler = new MethodCompiler(state)
      if (!getMethodStatic(method))
        methodCompiler.variables.add("this", ObjectTypeC.objectType(classCompiler.currentClassInfo.name))
      for (parameter <- parameters)
        methodCompiler.variables.add(getParameterName(parameter), getParameterType(parameter, classCompiler))
      getState(state).methodCompiler = methodCompiler
    }
  }

  def getMethodReturnType(metaObject: MetaObject) = {
    metaObject(ReturnTypeKey).asInstanceOf[MetaObject]
  }

  def getMethodParameters(metaObject: MetaObject) = {
    metaObject(MethodParametersKey).asInstanceOf[Seq[MetaObject]]
  }


  def addMethodFlags(method: MetaObject) = {
    var flags = Set[ByteCodeMethodInfo.MethodAccessFlag]()
    if (getMethodStatic(method))
      flags += ByteCodeMethodInfo.StaticAccess

    getMethodVisibility(method) match {
      case MethodC.PublicVisibility => flags += ByteCodeMethodInfo.PublicAccess
      case MethodC.PrivateVisibility => flags += ByteCodeMethodInfo.PrivateAccess
      case MethodC.DefaultVisibility => flags += ByteCodeMethodInfo.PrivateAccess
    }

    method(ByteCodeMethodInfo.AccessFlagsKey) = flags
  }

  def getMethodVisibility(method: MetaObject) = method(VisibilityKey).asInstanceOf[Visibility]

  def getMethodStatic(method: MetaObject) = method(StaticKey).asInstanceOf[Boolean]

  def getMethodCompiler(state: TransformationState) = getState(state).methodCompiler

  private def getState(state: TransformationState): State = {
    state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]
  }

  def getMethodBody(metaObject: MetaObject) = metaObject(MethodBodyKey).asInstanceOf[Seq[MetaObject]]

  def getMethodName(method: MetaObject) = {
    method(MethodNameKey).asInstanceOf[String]
  }

  def getMethods(clazz: MetaObject) = ClassC.getMembers(clazz).filter(member => member.clazz == MethodKey)

  def getParameterName(metaObject: MetaObject) = metaObject(ParameterNameKey).asInstanceOf[String]

  object ParametersGrammar
  object VisibilityGrammar
  object StaticGrammar
  object ReturnTypeGrammar

  override def transformGrammars(grammars: GrammarCatalogue) {
    val block = grammars.find(BlockC.BlockGrammar)

    val parseType = grammars.find(TypeC.TypeGrammar)
    val parseReturnType = grammars.create(ReturnTypeGrammar, "void" ~> produce(VoidTypeC.voidType) | parseType)

    val parseParameter = parseType ~~ identifier ^^ parseMap(ParameterKey, ParameterTypeKey, ParameterNameKey)
    val parseParameters = grammars.create(ParametersGrammar, "(" ~> parseParameter.manySeparated(",") <~ ")")
    val parseStatic = grammars.create(StaticGrammar, "static" ~> produce(true) | produce(false))

    val visibilityModifier = grammars.create(VisibilityGrammar,
      "public" ~> produce(PublicVisibility) |
        "protected" ~> produce(ProtectedVisibility) |
        "private" ~> produce(PrivateVisibility) |
        produce(DefaultVisibility))

    val methodGrammar = grammars.create(MethodGrammar, visibilityModifier ~~ parseStatic ~~ parseReturnType ~~ identifier ~ parseParameters % block ^^
      parseMap(MethodKey, VisibilityKey, StaticKey, ReturnTypeKey, MethodNameKey, MethodParametersKey, MethodBodyKey))

    val memberGrammar = grammars.find(ClassC.ClassMemberGrammar)
    memberGrammar.addOption(methodGrammar)
  }

  def method(name: String, _returnType: Any, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
             static: Boolean = false, visibility: Visibility = PrivateVisibility) = {
    new MetaObject(MethodKey) {
      data.put(MethodNameKey, name)
      data.put(ReturnTypeKey, _returnType)
      data.put(MethodParametersKey, _parameters)
      data.put(MethodBodyKey, _body)
      data.put(StaticKey, static)
      data.put(VisibilityKey, visibility)
    }
  }

  object ParameterKey
  def parameter(name: String, _type: Any) = {
    new MetaObject(ParameterKey) {
      data.put(ParameterNameKey, name)
      data.put(ParameterTypeKey, _type)
    }
  }

  class State() {
    var methodCompiler: MethodCompiler = null
  }

  class Visibility

  object MethodKey

  object MethodGrammar

  object MethodBodyKey

  object ParameterNameKey

  object StaticKey

  object VisibilityKey

  object ReturnTypeKey

  object MethodNameKey

  object MethodParametersKey


  object ParameterTypeKey

  object PublicVisibility extends Visibility

  object ProtectedVisibility extends Visibility

  object PrivateVisibility extends Visibility

  object DefaultVisibility extends Visibility

}
