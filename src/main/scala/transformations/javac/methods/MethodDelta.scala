package transformations.javac.methods

import core.bigrammar.{BiGrammar, TopBottom}
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.{Path, PathRoot}
import transformations.bytecode.ByteCodeMethodInfo._
import transformations.bytecode.attributes.CodeAttribute.{CodeAttributesKey, CodeExceptionTableKey, Instructions, CodeMaxLocalsKey}
import transformations.bytecode.attributes.{AttributeNameKey, CodeAttribute}
import transformations.bytecode.constants.Utf8ConstantDelta
import transformations.bytecode.extraConstants.TypeConstant
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.bytecode.types.{TypeSkeleton, VoidTypeC}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.classes.skeleton._
import transformations.javac.classes.{ClassCompiler, MethodInfo}
import transformations.javac.statements.{BlockC, StatementSkeleton}
import transformations.javac.types.{MethodType, TypeAbstraction}

object MethodDelta extends DeltaWithGrammar with WithCompilationState with ClassMemberDelta {

  implicit class Method(node: Node) {
    def returnType: Node = node(ReturnTypeKey).asInstanceOf[Node]
    def returnType_=(value: Node): Unit = node(ReturnTypeKey) = value

    def parameters: Seq[Node] = node(MethodParametersKey).asInstanceOf[Seq[Node]]
    def parameters_=(value: Seq[Node]): Unit = node(MethodParametersKey) = value
  }

  def compile(compilation: Compilation, clazz: Node): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methods = getMethods(clazz)
    clazz(ByteCodeSkeleton.ClassMethodsKey) = methods.map(method => {
      convertMethod(method, classCompiler, compilation)
      method
    })
  }

  def bind(compilation: Compilation, signature: ClassSignature, clazz: Node): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    val classInfo = classCompiler.currentClassInfo

    val methods = getMethods(clazz)
    for (method <- methods)
      bindMethod(method)

    def bindMethod(method: Node) = {
      val methodName: String = MethodDelta.getMethodName(method)
      val parameters = method.parameters
      val parameterTypes = parameters.map(p => getParameterType(p, classCompiler))
      val _type = MethodType.construct(method.returnType, parameterTypes)
      val key = new MethodClassKey(methodName, parameterTypes.toVector)
      classInfo.methods(key) = new MethodInfo(_type, MethodDelta.getMethodStatic(method))
    }
  }

  override def dependencies: Set[Contract] = Set(BlockC, InferredMaxStack, InferredStackFrames, JavaClassSkeleton)

  def getParameterType(metaObject: Node, classCompiler: ClassCompiler) = {
    val result = metaObject(ParameterTypeKey).asInstanceOf[Node]
    JavaClassSkeleton.fullyQualify(result, classCompiler)
    result
  }

  def getMethodDescriptor(method: Node, classCompiler: ClassCompiler): Node = {
    val returnType = getMethodReturnType(method)
    val parameters = getMethodParameters(method)
    val methodType = MethodType.construct(returnType, parameters.map(p => getParameterType(p, classCompiler)))
    TypeConstant.constructor(methodType)
  }

  def convertMethod(method: Node, classCompiler: ClassCompiler, compilation: Compilation): Unit = {

    method.clazz = ByteCodeMethodInfo.MethodInfoKey
    addMethodFlags(method)
    method(ByteCodeMethodInfo.MethodNameIndex) = Utf8ConstantDelta.create(getMethodName(method))
    method.data.remove(MethodNameKey)
    val methodDescriptorIndex = getMethodDescriptor(method, classCompiler)
    method(ByteCodeMethodInfo.MethodDescriptor) = methodDescriptorIndex
    addCodeAnnotation(PathRoot(method))
    method.data.remove(ReturnTypeKey)
    method.data.remove(MethodParametersKey)

    def addCodeAnnotation(method: Path) {
      setMethodCompiler(method, compilation)
      val statements = getMethodBody(method)
      method.current.data.remove(MethodBodyKey)
      val statementToInstructions = StatementSkeleton.getToInstructions(compilation)
      val instructions = statements.flatMap(statement => statementToInstructions(statement))
      val exceptionTable = Seq[Node]()
      val codeAttributes = Seq[Node]()
      val maxLocalCount: Int = getMethodCompiler(compilation).variablesPerStatement.values.map(pool => pool.localCount).max //TODO move this to a lower level.
      val codeAttribute = new Node(CodeAttribute.CodeKey,
        AttributeNameKey -> CodeAttribute.constantEntry,
        CodeMaxLocalsKey -> maxLocalCount,
        Instructions -> instructions,
        CodeExceptionTableKey -> exceptionTable,
        CodeAttributesKey -> codeAttributes)
      method(ByteCodeMethodInfo.MethodAttributes) = Seq(codeAttribute)
    }
  }

  def setMethodCompiler(method: Node, compilation: Compilation) {
    val methodCompiler = new MethodCompiler(compilation, method)
    getState(compilation).methodCompiler = methodCompiler
  }

  def getMethodReturnType(metaObject: Node) = {
    metaObject(ReturnTypeKey).asInstanceOf[Node]
  }

  def getMethodParameters(metaObject: Node) = {
    metaObject(MethodParametersKey).asInstanceOf[Seq[Node]]
  }

  def addMethodFlags(method: Node) = {
    var flags = Set[ByteCodeMethodInfo.MethodAccessFlag]()
    if (getMethodStatic(method))
      flags += ByteCodeMethodInfo.StaticAccess

    flags ++= visibilityToAccessFlag(getMethodVisibility(method))

    method(ByteCodeMethodInfo.AccessFlagsKey) = flags
  }

  val visibilityToAccessFlag = visibilityAccessFlagLinks.toMap
  def visibilityAccessFlagLinks: Seq[(Visibility, Set[ByteCodeMethodInfo.MethodAccessFlag])] = Seq(
    (PublicVisibility, Set[MethodAccessFlag](PublicAccess)),
    (PrivateVisibility, Set[MethodAccessFlag](PrivateAccess)),
    (DefaultVisibility, Set.empty[MethodAccessFlag])
  )

  def getMethodVisibility(method: Node) = method(VisibilityKey).asInstanceOf[Visibility]

  def getMethodStatic(method: Node) = method(StaticKey).asInstanceOf[Boolean]

  def getMethodCompiler(compilation: Compilation) = getState(compilation).methodCompiler

  def getMethodBody[T <: NodeLike](metaObject: T) = metaObject(MethodBodyKey).asInstanceOf[Seq[T]]

  def getMethodName(method: Node) = {
    method(MethodNameKey).asInstanceOf[String]
  }

  def getMethods(clazz: Node) = clazz.members.filter(member => member.clazz == MethodKey)

  def getParameterName(metaObject: Node) = metaObject(ParameterNameKey).asInstanceOf[String]

  object ParametersGrammar extends GrammarKey
  object VisibilityGrammar extends GrammarKey
  object StaticGrammar extends GrammarKey
  object ReturnTypeGrammar extends GrammarKey

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit =  {
    import grammars._
    val block = find(BlockC.BlockGrammar)

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    val parseReturnType = create(ReturnTypeGrammar, "void" ~> value(VoidTypeC.voidType) | parseType)

    val parseParameter = parseType.as(ParameterTypeKey) ~~ identifier.as(ParameterNameKey) asNode ParameterKey
    val parseParameters = create(ParametersGrammar, "(" ~> parseParameter.manySeparated(",") ~< ")")
    val parseStatic = create(StaticGrammar, "static" ~~> value(true) | value(false))

    val visibilityModifier = create(VisibilityGrammar,
      "public" ~~> value(PublicVisibility) |
        "protected" ~~> value(ProtectedVisibility) |
        "private" ~~> value(PrivateVisibility) |
        value(DefaultVisibility))


    val typeParametersGrammar: BiGrammar = find(TypeAbstraction.TypeParametersGrammar)

    val methodUnmapped: TopBottom = visibilityModifier.as(VisibilityKey) ~ parseStatic.as(StaticKey) ~ typeParametersGrammar.as(TypeParameters) ~
      parseReturnType.as(ReturnTypeKey) ~~ identifier.as(MethodNameKey) ~ parseParameters.as(MethodParametersKey) % block.as(MethodBodyKey)
    val methodGrammar = create(MethodGrammar, methodUnmapped.asNode(MethodKey))

    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    memberGrammar.addOption(methodGrammar)
  }

  def method(name: String, _returnType: Any, _parameters: Seq[Node], _body: Seq[Node],
             static: Boolean = false, visibility: Visibility = PrivateVisibility, typeParameters: Seq[Node] = Seq.empty) = {
    new Node(MethodKey,
      MethodNameKey -> name,
      ReturnTypeKey -> _returnType,
      MethodParametersKey -> _parameters,
      MethodBodyKey -> _body,
      StaticKey -> static,
      VisibilityKey -> visibility,
      TypeParameters -> typeParameters)
  }

  object ParameterKey extends NodeClass
  def parameter(name: String, _type: Any) = {
    new Node(ParameterKey,
      ParameterNameKey -> name,
      ParameterTypeKey -> _type)
  }

  def createState = new State()
  class State {
    var methodCompiler: MethodCompiler = _
  }

  class Visibility extends NodeClass

  object MethodKey extends NodeClass

  object MethodGrammar extends GrammarKey

  object MethodBodyKey extends NodeField

  object ParameterNameKey extends NodeField

  object StaticKey extends NodeField

  object VisibilityKey extends NodeField

  object ReturnTypeKey extends NodeField

  object MethodNameKey extends NodeField

  object MethodParametersKey extends NodeField

  object TypeParameters extends NodeField

  object ParameterTypeKey extends NodeField

  object PublicVisibility extends Visibility

  object ProtectedVisibility extends Visibility

  object PrivateVisibility extends Visibility

  object DefaultVisibility extends Visibility

  override def description: String = "Enables Java classes to contain methods."
}
