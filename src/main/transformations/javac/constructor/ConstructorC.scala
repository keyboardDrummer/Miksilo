package transformations.javac.constructor

import core.exceptions.BadInputException
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.{GrammarTransformation, ParticleWithPhase}
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeMethodInfo
import transformations.bytecode.coreInstructions.InvokeSpecialC
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.classes._
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.javac.methods.MethodC._
import transformations.javac.methods.{CallC, MethodC}
import transformations.javac.statements.{BlockC, StatementC}
import transformations.types.VoidTypeC

object ConstructorC extends GrammarTransformation with ParticleWithPhase {
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(ClassC, CallC, InvokeSpecialC, LoadAddressC)

  object SuperCallExpression extends ExpressionInstance {
    override val key: AnyRef = SuperCall

    override def getType(expression: MetaObject, state: TransformationState): MetaObject = VoidTypeC.voidType

    override def toByteCode(call: MetaObject, state: TransformationState): Seq[MetaObject] = {
      val classCompiler = ClassC.getClassCompiler(state)
      transformSuperOrThisCall(classCompiler.currentClass, call, state)
    }

    override def transformGrammars(grammars: GrammarCatalogue): Unit = {
      val callArguments = grammars.find(CallC.CallArgumentsGrammar)
      val superCallGrammar = "super" ~> callArguments ^^ parseMap(SuperCall, CallC.CallArguments)
      val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
      expressionGrammar.addOption(superCallGrammar)
    }
  }

  object ThisCallExpression extends ExpressionInstance {
    override val key: AnyRef = ThisCall

    override def getType(expression: MetaObject, state: TransformationState): MetaObject = VoidTypeC.voidType

    override def toByteCode(call: MetaObject, state: TransformationState): Seq[MetaObject] = {
      val classCompiler = ClassC.getClassCompiler(state)
      transformSuperOrThisCall(classCompiler.currentClass, call, state)
    }

    override def transformGrammars(grammars: GrammarCatalogue): Unit = {
      val callArguments = grammars.find(CallC.CallArgumentsGrammar)
      val thisCallGrammar = "this" ~> callArguments ^^ parseMap(ThisCall, CallC.CallArguments)
      val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
      expressionGrammar.addOption(thisCallGrammar)
    }
  }

  def transformSuperOrThisCall(clazz: MetaObject, call: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val compiler = ClassC.getClassCompiler(state)
    val callArguments = CallC.getCallArguments(call)
    val className = call.clazz match {
      case SuperCall => ClassC.getParent(clazz).get
      case ThisCall => ClassC.getClassName(clazz)
    }
    val qualifiedName = compiler.fullyQualify(className)
    val methodRefIndex = compiler.getMethodRefIndex(new MethodId(qualifiedName, constructorName))
    val argumentInstructions = callArguments.flatMap(argument => StatementC.getToInstructions(state)(argument))
    Seq(LoadAddressC.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialC.invokeSpecial(methodRefIndex))
  }

  override def inject(state: TransformationState): Unit = {
    ThisCallExpression.inject(state)
    SuperCallExpression.inject(state)
    super.inject(state)
  }

  case class BadConstructorNameException(clazz: MetaObject, constructor: MetaObject) extends BadInputException

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    val className = ClassC.getClassName(clazz)
    for (constructor <- ClassC.getMethods(clazz).filter(method => method.clazz == ConstructorKey)) {
      val constructorClassName = constructor(ConstructorClassNameKey).asInstanceOf[String]
      if (!constructorClassName.equals(className))
        throw new BadConstructorNameException(clazz, constructor)

      constructor.clazz = ByteCodeMethodInfo.MethodInfoKey
      constructor(MethodC.MethodNameKey) = constructorName
      constructor(MethodC.ReturnTypeKey) = VoidTypeC.voidType
      constructor(StaticKey) = false
    }
  }

  def superCall(arguments: Seq[MetaObject] = Seq()) = new MetaObject(SuperCall) {
    data.put(CallC.CallArguments, arguments)
  }

  def thisCall(arguments: Seq[MetaObject]) = new MetaObject(ThisCall) {
    data.put(CallC.CallArguments, arguments)
  }

  def constructor(className: String, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
                  visibility: Visibility = PublicVisibility) = new MetaObject(ConstructorKey,
    MethodParametersKey -> _parameters, MethodBodyKey -> _body, VisibilityKey -> visibility,
    ConstructorClassNameKey -> className)

  object SuperCall

  object ThisCall

  object ConstructorKey

  object ConstructorClassNameKey

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val memberGrammar = grammars.find(ClassC.ClassMemberGrammar)
    val visibilityModifier = grammars.find(MethodC.VisibilityGrammar)
    val parseParameters = grammars.find(MethodC.ParametersGrammar)
    val block = grammars.find(BlockC.BlockGrammar)
    val constructorGrammar = visibilityModifier ~~ identifier ~ parseParameters % block ^^
      parseMap(ConstructorKey, VisibilityKey, ConstructorClassNameKey, MethodParametersKey, MethodBodyKey)
    memberGrammar.addOption(constructorGrammar)
  }
}
