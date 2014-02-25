package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.{JavaMethodModel, JavaClassModel}

object ImplicitSuperConstructorCall extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ConstructorC)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {

    for(constructor <- JavaClassModel.getMethods(clazz).filter(method => method.clazz == ConstructorC.Constructor))
    {
      val statements = JavaMethodModel.getMethodBody(constructor)
      var addSuperCall = false
      if (statements.isEmpty)
        addSuperCall = true
      else
      {
        val firstStatement = statements(0)
        if (firstStatement.clazz != ConstructorC.SuperCall && firstStatement.clazz != ConstructorC.ThisCall)
        {
          addSuperCall = true
        }
      }

      if (addSuperCall)
        constructor(JavaMethodModel.MethodBodyKey) = Seq(ConstructorC.superCall()) ++ statements
    }
  }
}
