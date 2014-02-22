package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.java.base._
import languages.java.base.JavaMethodModel._
import languages.bytecode.ByteCode

object ConstructorC extends ProgramTransformation {
  val constructorName: String = "init"
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(SuperCall,(superCall : MetaObject, compiler) => {
      Seq(ByteCode.addressLoad(0),ByteCode.invokeSpecial(???))
    })
    JavaBase.getStatementToLines(state).put(ThisCall,(superCall : MetaObject, compiler) => {
      Seq(ByteCode.addressLoad(0),ByteCode.invokeSpecial(???))
    })

    val className = JavaClassModel.getClassName(clazz)
    for(constructor <- JavaClassModel.getMethods(clazz).filter(method => method.clazz == Constructor))
    {
      constructor(JavaMethodModel.MethodNameKey) = constructorName
      constructor(JavaMethodModel.ReturnTypeKey) = JavaTypes.objectType(className)
    }
  }

  object SuperCall
  object ThisCall
  def superCall(arguments: Seq[MetaObject]) = new MetaObject(SuperCall) {
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
