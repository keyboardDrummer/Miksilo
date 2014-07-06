package transformations.javac

import core.transformation.{TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base._
import transformations.javac.base.model.{JavaMethodModel, JavaTypes, JavaClassModel, JavaBaseModel}
import JavaMethodModel._
import transformations.bytecode.ByteCode
import transformations.javac.base.MethodCompiler
import transformations.javac.base.model.{JavaTypes, JavaClassModel, JavaBaseModel}
import JavaBaseModel._
import transformations.javac.base.MethodCompiler
import transformations.javac.base.MethodId

object ConstructorC extends ProgramTransformation {
  val constructorName: String = "<init>"
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    def transformSuperOrThisCall(call: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
      val callArguments = getCallArguments(call)
      val className = call.clazz match {
        case SuperCall => JavaClassModel.getParent(clazz).get
        case ThisCall => JavaClassModel.getClassName(clazz)
      }
      val qualifiedName = compiler.classCompiler.fullyQualify(className)
      val methodRefIndex = compiler.classCompiler.getMethodRefIndex(new MethodId(qualifiedName, constructorName))
      val argumentInstructions = callArguments.flatMap(argument => JavaBase.statementToInstructions(argument, compiler))
      Seq(ByteCode.addressLoad(0)) ++ argumentInstructions ++ Seq(ByteCode.invokeSpecial(methodRefIndex))
    }

    JavaBase.getStatementToLines(state).put(SuperCall, transformSuperOrThisCall)
    JavaBase.getStatementToLines(state).put(ThisCall, transformSuperOrThisCall)

    for(constructor <- JavaClassModel.getMethods(clazz).filter(method => method.clazz == Constructor))
    {
      constructor.clazz = ByteCode.MethodInfoKey
      constructor(JavaMethodModel.MethodNameKey) = constructorName
      constructor(JavaMethodModel.ReturnTypeKey) = JavaTypes.VoidType
      constructor(StaticKey) = false
    }
  }


  object SuperCall
  object ThisCall
  def superCall(arguments: Seq[MetaObject] = Seq()) = new MetaObject(SuperCall) {
    data.put(JavaBaseModel.CallArguments, arguments)
  }

  def thisCall(arguments: Seq[MetaObject]) = new MetaObject(ThisCall) {
    data.put(JavaBaseModel.CallArguments, arguments)
  }

  object Constructor
  def constructor(_parameters: Seq[MetaObject], _body: Seq[MetaObject], visibility: Visibility = PublicVisibility) = new MetaObject(Constructor) {
    data.put(MethodParametersKey, _parameters)
    data.put(MethodBodyKey,_body)
    data.put(VisibilityKey, visibility)
  }
}
