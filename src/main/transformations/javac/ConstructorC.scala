package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.coreInstructions.{InvokeSpecialC, LoadAddressC}
import transformations.javac.classes._
import transformations.javac.methods.MethodC._
import transformations.javac.methods.{CallC, MethodC}
import transformations.javac.statements.StatementC
import transformations.types.VoidTypeC

object ConstructorC extends ProgramTransformation {
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(ClassC, CallC, InvokeSpecialC, LoadAddressC)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    def transformSuperOrThisCall(call: MetaObject): Seq[MetaObject] = {
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

    StatementC.getStatementToLines(state).put(SuperCall, transformSuperOrThisCall)
    StatementC.getStatementToLines(state).put(ThisCall, transformSuperOrThisCall)

    for (constructor <- ClassC.getMethods(clazz).filter(method => method.clazz == Constructor)) {
      constructor.clazz = ByteCodeSkeleton.MethodInfoKey
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

  def constructor(_parameters: Seq[MetaObject], _body: Seq[MetaObject], visibility: Visibility = PublicVisibility) = new MetaObject(Constructor) {
    data.put(MethodParametersKey, _parameters)
    data.put(MethodBodyKey, _body)
    data.put(VisibilityKey, visibility)
  }

  object SuperCall

  object ThisCall

  object Constructor

}
