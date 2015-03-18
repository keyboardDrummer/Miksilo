package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{MetaObject, MetaLikeGen, Key}
import core.particles.path.{Path, Root}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.attributes.CodeAttribute.{CodeAttributesKey, CodeExceptionTableKey, CodeInstructionsKey, CodeMaxLocalsKey}
import transformations.bytecode.attributes.{CodeAttribute, CodeConstantEntry}
import transformations.bytecode.constants.MethodDescriptorConstant
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.classes.{ClassCompiler, JavaClassSkeleton, MethodInfo}
import transformations.javac.statements.{BlockC, StatementSkeleton}
import transformations.types.{TypeSkeleton, VoidTypeC}

object MethodC extends ParticleWithGrammar with WithState {

  override def inject(state: CompilationState): Unit = {
    super.inject(state)

    JavaClassSkeleton.getState(state).firstMemberPasses ::= (clazz => bindMethods(state, clazz))
    JavaClassSkeleton.getState(state).secondMemberPasses ::= (clazz => convertMethods(state, clazz))
  }

  def convertMethods(state: CompilationState, clazz: MetaObject) = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)

    val methods = getMethods(clazz)
    clazz(ByteCodeSkeleton.ClassMethodsKey) = methods.map(method => {
      convertMethod(method, classCompiler, state)
      method
    })
  }

  def bindMethods(state: CompilationState, clazz: MetaObject): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)
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

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames, JavaClassSkeleton)

  def getParameterType(metaObject: MetaObject, classCompiler: ClassCompiler) = {
    val result = metaObject(ParameterTypeKey).asInstanceOf[MetaObject]
    JavaClassSkeleton.fullyQualify(result, classCompiler)
    result
  }

  def getMethodDescriptor(method: MetaObject, classCompiler: ClassCompiler): MetaObject = {
    val returnType = getMethodReturnType(method)
    val parameters = getMethodParameters(method)
    MethodDescriptorConstant.methodDescriptor(returnType, parameters.map(p => getParameterType(p, classCompiler)))
  }

  def convertMethod(method: MetaObject, classCompiler: ClassCompiler, state: CompilationState): Unit = {
    val constantPool = ByteCodeSkeleton.getState(state).constantPool
    def getMethodDescriptorIndex(method: MetaObject): Int = constantPool.store(getMethodDescriptor(method, classCompiler))

    method.clazz = ByteCodeMethodInfo.MethodInfoKey
    addMethodFlags(method)
    val methodNameIndex: Int = classCompiler.getNameIndex(getMethodName(method))
    method(ByteCodeMethodInfo.MethodNameIndex) = methodNameIndex
    method.data.remove(MethodNameKey)
    val methodDescriptorIndex = getMethodDescriptorIndex(method)
    method(ByteCodeMethodInfo.MethodDescriptorIndex) = methodDescriptorIndex
    addCodeAnnotation(new Root(method))
    method.data.remove(ReturnTypeKey)
    method.data.remove(MethodParametersKey)

    def addCodeAnnotation(method: Path) {
      setMethodCompiler(method, state)
      val statements = getMethodBody(method)
      method.current.data.remove(MethodBodyKey)
      val statementToInstructions = StatementSkeleton.getToInstructions(state)
      val instructions = statements.flatMap(statement => statementToInstructions(statement))
      val codeIndex = constantPool.store(CodeConstantEntry.entry)
      val exceptionTable = Seq[MetaObject]()
      val codeAttributes = Seq[MetaObject]()
      val maxLocalCount: Int = getMethodCompiler(state).variablesPerStatement.values.map(pool => pool.localCount).max //TODO move this to a lower level.
      val codeAttribute = new MetaObject(CodeAttribute.CodeKey,
        AttributeNameKey -> codeIndex,
        CodeMaxLocalsKey -> maxLocalCount,
        CodeInstructionsKey -> instructions,
        CodeExceptionTableKey -> exceptionTable,
        CodeAttributesKey -> codeAttributes)
      method(ByteCodeMethodInfo.MethodAttributes) = Seq(codeAttribute)
    }
  }

  def setMethodCompiler(method: MetaObject, state: CompilationState) {
    val methodCompiler = new MethodCompiler(state, method)
    getState(state).methodCompiler = methodCompiler
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

  def getMethodCompiler(state: CompilationState) = getState(state).methodCompiler

  def getMethodBody[T <: MetaLikeGen[T]](metaObject: T) = metaObject(MethodBodyKey).asInstanceOf[Seq[T]]

  def getMethodName(method: MetaObject) = {
    method(MethodNameKey).asInstanceOf[String]
  }

  def getMethods(clazz: MetaObject) = JavaClassSkeleton.getMembers(clazz).filter(member => member.clazz == MethodKey)

  def getParameterName(metaObject: MetaObject) = metaObject(ParameterNameKey).asInstanceOf[String]

  object ParametersGrammar
  object VisibilityGrammar
  object StaticGrammar
  object ReturnTypeGrammar

  override def transformGrammars(grammars: GrammarCatalogue) {
    val block = grammars.find(BlockC.BlockGrammar)

    val parseType = grammars.find(TypeSkeleton.TypeGrammar)
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

    val memberGrammar = grammars.find(JavaClassSkeleton.ClassMemberGrammar)
    memberGrammar.addOption(methodGrammar)
  }

  def method(name: String, _returnType: Any, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
             static: Boolean = false, visibility: Visibility = PrivateVisibility) = {
    new MetaObject(MethodKey,
      MethodNameKey -> name,
      ReturnTypeKey -> _returnType,
      MethodParametersKey -> _parameters,
      MethodBodyKey -> _body,
      StaticKey -> static,
      VisibilityKey -> visibility)
  }

  object ParameterKey
  def parameter(name: String, _type: Any) = {
    new MetaObject(ParameterKey,
      ParameterNameKey -> name,
      ParameterTypeKey -> _type)
  }

  def createState = new State()
  class State() {
    var methodCompiler: MethodCompiler = null
  }

  class Visibility extends Key

  object MethodKey

  object MethodGrammar

  object MethodBodyKey extends Key

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

  override def description: String = "Enables Java classes to contain methods."
}
